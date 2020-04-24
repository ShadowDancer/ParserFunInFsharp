module ParserCombinators.Core

open TextInput


type ParserLabel = string
type ParserError = string
type Result<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserError * ParserPosition 

let printResult result =
    match result with
    | Success (value,_input) -> 
        printfn "%A" value
    | Failure (label,error,position) ->
        let errorHeader = 
            sprintf "Line %d, Col %d, Error parsing %s" position.line position.column label
        let errorCarret = 
            sprintf "%*s^%s" position.column "" error
        printf "%s\n%s\n%s\n" errorHeader position.currentLine errorCarret
        
type ParserFn<'T> = InputState -> Result<'T * InputState>
type Parser<'T> = 
    { 
        parseFn:(ParserFn<'T>); 
        label:ParserLabel
    }

// Creates parse out of constants
let returnP v : Parser<'a> =
    let label = sprintf "%A" v
    let innerFn input = 
        Success (v, input)
    {parseFn=innerFn; label=label}

let run (parser:Parser<'T>) input : Result<'T * InputState> = 
    parser.parseFn input

let runParser parser input =
    match run parser input with
    | Success (value, remaining) ->
        value
    | Failure (a, b, c) ->
        printResult(Failure (a, b, c))
        raise (invalidOp ("Failed parsing " + a + " " + b ))
        

let getLabel parser =
    parser.label

/// returns parser with given label
let setLabel parser newLabel = 
    let newInnerFn input = 
        let result = parser.parseFn input
        match result with
        | Success s ->
            Success s 
        | Failure (_oldLabel, err, pos) -> 
            Failure (newLabel, err, pos)       
    {parseFn=newInnerFn; label=newLabel} 

/// returns new parser with given label
let ( <?> ) = setLabel
    
let satisfy predicate label=
    let innerFn input= 
        let remaining, token = nextToken input
        match token with
        | None ->
            let pos = parserPositionFromInputState input
            Failure (label, "Input is empty", pos)
        | Some inputToken ->
            if  predicate inputToken then
                Success (inputToken, remaining)
            else
                let err = sprintf "Unexpected '%c'" inputToken
                let pos = parserPositionFromInputState input
                Failure (label, err, pos)
    {parseFn=innerFn; label=label}

let andThen parserA parserB =
    let newLabel =
        sprintf "%s andThen %s" parserA.label parserB.label

    let innerFn input =                
        match run parserA input with
        | Failure (label, error, pos) -> 
            Failure (label, error, pos)        
        | Success (resultA,remainingA) -> 
            match run parserB remainingA with 
            | Failure (label, error, pos) ->
                Failure (label, error, pos)
            | Success (resultB,remainingB) -> 
                let result = (resultA, resultB)
                Success (result,remainingB)
        
    { parseFn=innerFn ; label = newLabel}
let ( .>>. ) = andThen

// parserA or parserB    
let orElse (parserA:Parser<'T>) (parserB:Parser<'T>) =
    let newLabel = 
        sprintf "%s or %s" parserA.label parserB.label
    let innerFn input =
        match run parserA input with
        | Success successA -> 
            Success successA
        | Failure _ -> 
            run parserB input
    {parseFn=innerFn; label= newLabel}
let ( <|> ) = orElse

/// Maps value parsed by parser with function
let mapP bind parser =
    let innerFn input = 
        match run parser input with
        | Success (result, remaining) ->
            Success (bind result, remaining)
        | Failure (label, error, pos) ->
            Failure (label, error, pos)
    { parseFn = innerFn; label = ""}

/// Maps value parsed by parser with function
let ( <!> ) = mapP
 /// Maps value parsed by parser with function
let ( |>> ) x f = mapP f x

/// map to parameter
let mapToParam parser param =
    parser |>> fun _ -> param

/// map to parameter
let (>>%) = mapToParam

/// parses expression but throws away left result
let (>>.) leftParser rightParser =
    leftParser .>>. rightParser
    |>> snd

/// parses expression but throws away right result
let (.>>) leftParser rightParser =
    leftParser .>>. rightParser
    |>> fst

/// Applies function of parsers to parser
let applyP fP xP =
    (fP .>>. xP)
    |>> (fun (f,x) -> f x)
/// Applies parser of function to argument
let ( <*> ) = applyP

/// Convers 2 arg function to function parser of that function
let liftP2 f xP yP = 
    returnP f <*> xP <*> yP
    
/// helper function for m
let rec private parseZeroOrMore parser input = 
    match run parser input with
    | Success (value, remaining) ->
        let values, remaining = parseZeroOrMore parser remaining
        (value :: values, remaining)
    | Failure (_lable, _error, _pos) ->
        ([], input)
    
/// matches parser multiple times
let many parser =
    let innerFn input =
        Success (parseZeroOrMore parser input)
    {parseFn=innerFn; label = "sequence " + parser.label}
    
/// matches parser multiple times, at least once
let many1 parser =
    let innerFn input =
        let values, remaining = parseZeroOrMore parser input
        match values.Length with
        | 0 -> 
            let error = "Expected " + parser.label
            let pos = parserPositionFromInputState input
            Failure (parser.label, error, pos)
        | _ -> Success (values, remaining)
    {parseFn=innerFn; label = "sequence"}

let manyWithSep valueParser separatorParser =
    let innerFn input =
        match run valueParser input with
        | Success (value, remaining) ->
            let valueWithSeparator = (many (separatorParser >>. valueParser))
            match run valueWithSeparator remaining with
            | Success (values, manyRemaining) ->
                Success (value::values, manyRemaining)
            | Failure (label, error, pos) -> // many should never return failure
                Failure (label, error, pos)
        | Failure (_, _, _) ->
            Success ([], input)
    {parseFn=innerFn; label = "sequence with separators"}
    
let many1withSep valueParser separatorParser =
    let valueWithSepParser = separatorParser >>. valueParser
    valueParser .>>. many valueWithSepParser
    |>> fun (x, z) -> x::z
    <?> "sequence with separators"

/// matches parser multiple times, at least once
let opt parser =
    let innerFn input =
        match run parser input with
        | Success (value, remaining) ->
            Success (Some value, remaining)
        | Failure (_label, _error, _pos) ->
            Success (None, input)
    {parseFn=innerFn; label = "opt " + parser.label}

/// keep only results of middle praser
let between leftParser middleParser rightParser =
    leftParser >>. middleParser .>> rightParser
    