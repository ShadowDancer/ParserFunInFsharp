module Auxiliary

open Xunit
open Xunit.Sdk
open ParserCombinators.Core
open ParserCombinators.TextInput

let defaultLabel = "DefaultLabel"

/// Function which takes any parameter
let throwOnAny1 _x = raise (invalidOp "");

let exampleLine1 = "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
let exampleLine2 = "Proin nibh augue, suscipit a, scelerisque sed, lacinia in, mi."
let exampleLine3 = "Donec venenatis, turpis vel hendrerit interdum, dui ligula ultricies purus, sed posuere libero dui id orci."
let multilineText = sprintf "%s\n%s\n%s" exampleLine1 exampleLine2 exampleLine3

let singleLineInput = fromStr exampleLine1
let multiLineInput = fromStr multilineText

let failingParser = 
    let label = "failing parser"
    let parserFn input =
        let pos = parserPositionFromInputState input
        Failure (label, "This parser always fails", pos)
    {parseFn=parserFn; label=label}

let advanceInput times input=
    let advanceByOne input = 
        let newInput, _ = nextToken input
        newInput
    let ignore2ndArg = (fun x _ -> x)

    {1..times}
    |> Seq.fold (advanceByOne >> ignore2ndArg) input

let repeatChar times char =
    {1..times}
    |> Seq.map (fun _ -> char)
    |> Seq.toArray
    |> fun x -> new string(x)
    
// Assert parsing operation ends with success and given value
type Assert with 
    static member Success expected result= 
        match result with
        | Failure (_label, _message, _position) ->
            raise (new XunitException("Expected success, got failure"))
        | Success (actualValue, _remaining) ->
            Assert.True(obj.Equals(expected, actualValue), sprintf "Expected %A got %A" expected actualValue)

// Assert success with input
type Assert with 
    static member SuccessWithInput (expectedInput:InputState) result= 
        match result with
        | Failure (_label, _message, _position) ->
            raise (new XunitException("Expected success, got failure"))
        | Success (_value, remaining) ->
            Assert.Equal(expectedInput, remaining)

// Assert pasing operation ends with failure
type Assert with 
    static member Failure result= 
        match result with
        | Failure (_label, _message, _position) ->
            ()
        | Success (_, _) ->
            raise (new XunitException("Expected failure, got success"))
            
// Assert pasing operation ends with failure
type Assert with 
    static member FailureOnPos expectedPosition result= 
        match result with
        | Failure (_label, _message, actualPosition) ->
            Assert.Equal(expectedPosition, actualPosition)
        | Success (_, _) ->
            raise (new XunitException("Expected failure, got success"))
            