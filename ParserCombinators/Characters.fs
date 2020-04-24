module ParserCombinators.Characters

open Core
open System

let parseChar char =
    satisfy char.Equals (Char.ToString char)

/// parse a digit
let parseDigitChar = 
    let predicate = fun c -> c >= '0' && c <= '9'
    let label = "digit"
    satisfy predicate label 
    
/// parse a whitespace char
let parseWhitespaceChar = 
    let predicate = Char.IsWhiteSpace 
    let label = "whitespace"
    satisfy predicate label 
    
/// parse letter
let parseLetterChar = 
    let predicate = Char.IsLetter
    let label = "letter"
    satisfy predicate label

/// parse multiple characters
let parseString (pattern:string) =
    let toString = (fun a -> a.ToString())

    pattern
    |> Seq.map parseChar
    |> Seq.map ((<!>) toString)
    |> Seq.reduce (liftP2 (+))
    <?> pattern

/// parse
let parseNumber = 
    let setResultSign (sign, number) = 
        match sign with
        | None -> number
        | Some _ -> -number

    let parseDigits = 
        many1 parseDigitChar
        |>> (fun x -> new String(List.toArray x) |> int) 

    opt (parseChar '-') .>>. parseDigits
    |> (<*>) (returnP setResultSign)
    <?> "number"