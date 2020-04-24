module JsonParser.Parser

open ParserCombinators.TextInput
open ParserCombinators.Characters
open ParserCombinators.Core
open JsonParser.Model
open System

let internal pNull = 
    parseString "null"
    >>% JNull
    <?> "null"

let internal pBool = 
    let parseTrue = 
        parseString "true"
        >>% JBool true
        <?> "true"
    let parseFalse =
        parseString "false"
        >>% JBool false
        <?> "false"
    parseTrue <|> parseFalse
    <?> "bool"

let internal pJsonChar =
    let escapeChar = '\\'
    let pEscapedChar = 
        escapedChars
       |> Seq.map (fun x -> parseChar (escapeChar) >>. parseChar x)
       |> Seq.reduce (<|>)
    let pNotQuoteOrEscaped =
        let label = "char"
        satisfy (fun c -> c <> '"' && c <> escapeChar) label
    let parseHex = 
        ['0'..'9'] @ ['A'..'F'] @ ['a'..'f']
        |> Seq.map (fun c -> parseChar c)
        |> Seq.reduce (<|>)
    let pEncoded = 
        (parseChar (escapeChar) .>>. parseChar 'u') >>. parseHex .>>. parseHex .>>. parseHex .>>. parseHex
        |>> fun (((d1, d2), d3), d4) -> (new string([|d1; d2; d3; d4|]))
        |>> fun str -> Int32.Parse(str,Globalization.NumberStyles.HexNumber) |> char

    pNotQuoteOrEscaped <|> pEncoded <|> pEscapedChar
    <?> "character"

let internal pString = 
    let pQuote = parseChar '"'
    pQuote >>. many pJsonChar .>> pQuote
    |>> List.toArray
    |>> fun a -> JString (new string(a))
    <?> "string"

// parse single whitespace
let internal pWhitespace =
    whitespaceChars
    |> Seq.map (fun c -> parseChar c)
    |> Seq.reduce (<|>)
    <?> "whitespace"

// prase many whitespaces
let internal pWhitespaces = many pWhitespace

// parse json number
let internal pNumber =    
    let optStr v =
        opt v
        |>> fun z -> match z with
                        | Some v -> v
                        | None -> ""

    let pSign = 
        optStr (parseChar '-' |>> string)

    let pDigits = 
        many1 parseDigitChar
        |>> Seq.toArray
        |>> fun x -> new string(x)

    let pDecimals = 
         parseChar '.' .>>. pDigits
         |>> fun (x, y) -> string x + y
         |> optStr

    let pExponents =
        let pExponentChar = parseChar 'e' <|> parseChar 'E'
        let pExponentSign = (parseChar '+' |>> string )  <|> (parseChar '-' |>> string) <|> returnP "" 
        let pExponentCore = 
            pExponentChar .>>. pExponentSign .>>. pDigits
            |>> fun ((x, y), z) -> string x + y + z
        optStr pExponentCore

    pSign .>>. pDigits .>>. pDecimals .>>. pExponents
    |>> fun (((sign, digits), decimals), exponents) -> sign + digits + decimals + exponents
    |>> double
    |>> JNumber
    <?> "number"


// Create forwarded ref so we can create object dependent on itself
let createParserForwardedToRef<'a>() =
    let dummyParser= 
        let innerFn _input : Result<'a * InputState> = failwith "unfixed forwarded parser"
        {parseFn=innerFn; label="unknown"}
    // ref to placeholder Parser
    let parserRef = ref dummyParser
    // wrapper Parser
    let innerFn input = 
        // forward input to the placeholder
        run !parserRef input 
    let wrapperParser = {parseFn=innerFn; label="unknown"}
    wrapperParser, parserRef
let pValuePlaceholder,pValueRef = createParserForwardedToRef<JValue>()

// parse json array
let internal pArray =
    let pOpeningBrace = parseChar '[' .>> pWhitespaces
    let pClosingBrace = parseChar ']'
    let pSeparator = parseChar ',' 
    let pArrayElements = many1withSep pValuePlaceholder pSeparator
    let pEmptyBody = (pWhitespaces >>% [])
    pOpeningBrace >>. (pArrayElements <|> pEmptyBody) .>> pClosingBrace
    |>> JArray
    <?> "array"

// parse json object
let internal pObject = 
    let unpackString x =
        match x with
        | JString v -> v
        | _ -> ""

    let pOpeningBracket = parseChar '{' .>> pWhitespaces
    let pClosingBracket = parseChar '}'
    let pKey = pWhitespaces >>. (pString |>>  unpackString) .>> pWhitespaces
    let pKvp = pKey .>> parseChar ':' .>>. pValuePlaceholder
    let pSep = parseChar ','
    let pEmptyBody = (pWhitespaces >>% [])
    let pObjectEntries = many1withSep pKvp pSep
    pOpeningBracket >>. (pObjectEntries  <|> pEmptyBody) .>> pClosingBracket
    |>> Map.ofSeq
    |>> JObject
    <?> "object"

let jsonParser =
    let parseValCore =
        pString <|> pBool <|> pNull <|> pArray  <|> pNumber <|> pObject
    pWhitespaces >>. parseValCore .>> pWhitespaces
    
pValueRef := jsonParser
let parseString str = runParser jsonParser (fromStr str)