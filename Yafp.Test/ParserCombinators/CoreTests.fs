module Yasc.Test.ParserCombinators.CoreTests

open Xunit
open Xunit.Sdk
open ParserCombinators.Core
open ParserCombinators.TextInput
open Auxiliary

[<Fact>]
let Should_createParerFromValue_beCreatedBy_returnP () =
    // arrange 
    let value = 5;
    let testFn = returnP value
    let input = fromStr ""
    // act
    let result = run testFn input
    // assert
    Assert.Success value result

[<Fact>]
let Should_callParseFn_whem_calledRunonInput () =
    // arrange 
    let input = fromStr ""
    let successVal = "yay we win!"
    let parseFn input = 
        Success (successVal, input)
    let parser = {parseFn=parseFn; label=""}
    let testFn = run
    // act
    let result = testFn parser input
    // assert
    match result with
    | Success (resultVal, resultInput) ->
        Assert.Equal(successVal, resultVal)
        Assert.Equal(input, resultInput)
    | Failure (_, _, _) ->
        raise (new XunitException("Expected Success"))

[<Fact>]
let Should_returnLabelFromParser_when_callingGetLabel () =
    // arragne
    let parser = { parseFn = throwOnAny1; label= defaultLabel}
    // act
    let result = getLabel parser
    // assert
    Assert.Equal(defaultLabel, result)

[<Fact>]
let Should_returnParserWithNewLabel_when_callingSetLabel () =
    // arragne
    let startLabel = "startLabel"
    let targetLabel = "targetLabel"
    let parser = { parseFn = throwOnAny1; label= startLabel}
    let testFn = setLabel
    // act
    let result = testFn parser targetLabel
    // assert
    Assert.Equal(targetLabel, getLabel result)

[<Fact>]
let Should_returnParserWithNewLabel_when_callingSetLabelOperator () =
    // arragne
    let startLabel = "startLabel"
    let targetLabel = "targetLabel"
    let parser = { parseFn = throwOnAny1; label= startLabel}
    let testFn = (<?>)
    // act
    let result = testFn parser targetLabel
    // assert
    Assert.Equal(targetLabel, getLabel result)
