module JsonParser.Unparser

open JsonParser.Model
open System.IO
open System.Globalization

type UnparseConfig = { pretty: bool; spaces: int; indent:int; stream: StreamWriter }

let indent config =
    {config with spaces = config.spaces + config.indent }

let (>>) (obj:string) (stream:StreamWriter) = stream.Write(obj)

let printIndent config =
    for _ in 1..config.spaces do " " >> config.stream

let internal upNull stream =
    "null" >> stream

let internal upBool stream jBool =
    match jBool with
    | true -> "true" >> stream
    | false -> "false" >> stream

let internal numberFormat =
    let nfi = new NumberFormatInfo()
    nfi.NumberDecimalSeparator <- "."
    nfi

let internal upNumber stream (number:double) =
    number.ToString(numberFormat) >> stream

let internal upString stream str =
    "\"" >> stream
    str >> stream
    "\"" >> stream



let rec internal upValue config jsonValue =
    match jsonValue with
    | JNull -> upNull config.stream
    | JBool b -> upBool config.stream b
    | JNumber num -> upNumber config.stream num
    | JString str -> upString config.stream str
    | JArray array -> upArray config array
    | JObject object -> upObject config object
and internal upArray config array =
    let elemConfig = indent config
    let printElement sep e = 
        if sep then 
            "," >> elemConfig.stream
            if elemConfig.pretty then "\n" >> elemConfig.stream
        printIndent elemConfig
        upValue elemConfig e
    match array with
    | [] -> "[]" >> config.stream
    | a -> printIndent config
           "[" >> config.stream
           if config.pretty then "\n" >> config.stream
           printElement false a.[0]
           Seq.map (printElement true) a.[1..] |> ignore
           printIndent config
           "]" >> config.stream
and internal upObject config object =
    let elemConfig = indent config
    let printElement sep (key, value) = 
        if sep then 
            "," >> elemConfig.stream
            if elemConfig.pretty then "\n" >> elemConfig.stream
        printIndent elemConfig
        key >> elemConfig.stream
        ": " >> elemConfig.stream
        upValue elemConfig value
    match object with
    | x when Map.isEmpty x -> "{}" >> config.stream
    | map ->   printIndent config
               "{" >> config.stream
               if config.pretty then "\n" >> config.stream
               let items = Map.toArray map
               printElement false items.[0]
               Seq.map (printElement true) items.[1..] |> ignore
               printIndent config
               "}" >> config.stream                    

let prettify stream json =
    let config = {pretty=true; spaces=0; indent=2; stream=stream}
    upValue config json

let minify stream json =
    let config = {pretty=false; spaces=0; indent=0; stream=stream}
    upValue config json