module Yasc.Test.ParserCombinators.CoreParseFunctionalTests

open Xunit
open Xunit.Sdk
open ParserCombinators.Core
open ParserCombinators.TextInput
open ParserCombinators.Characters
open Auxiliary

[<Fact>]
let Should_parserInternalsBeMapped_when_mapPcalled () =
    // arrange
    let char = 'a'
    let parser = parseChar char
    let input = fromStr (char.ToString())
    let mapFunction = int
    let testFn = mapP mapFunction parser
    // act
    let result = run testFn input
    // assert
    let expectedValue = mapFunction char
    Assert.Success expectedValue result

[<Fact>]
let Should_mapPfail_when_parserFails () =
    // arrange
    let parser = failingParser
    let testFn = mapP (fun x -> x.ToString()) parser
    let inputStr = exampleLine1
    let input = fromStr inputStr
    // act
    let result = run testFn input
    // assert
    let expectedPos = { line=0; column=0; currentLine=inputStr }
    Assert.FailureOnPos expectedPos result
    

// TODO: test apply
[<Fact>]
let Should_emptyListBeReturned_when_parserInManyFails () =
    // arrange    
    let parser = failingParser
    let testFn = many parser
    // act
    let result = run testFn singleLineInput
    // assert
    let expectedValue = []
    Assert.Success expectedValue result
    
[<Theory>]
[<InlineData(1)>]
[<InlineData(2)>]
[<InlineData(5)>]
[<InlineData(30)>]
let Should_parseMultipleTokens_when_callingMany times =
    // arrange    
    let parser = parseChar 'a'
    let inputStr = repeatChar times 'a' + "b"
    let input = fromStr inputStr
    let testFn = many parser
    // act
    let result = run testFn input
    // assert
    let expectedValues = 
        {1..times}
        |> Seq.map (fun i -> 'a')
        |> Seq.toList
    Assert.Success expectedValues result

[<Theory>]
[<InlineData(1)>]
[<InlineData(2)>]
[<InlineData(5)>]
[<InlineData(30)>]
let Should_parseMultipleTokens_when_callingMany1 times =
    // arrange    
    let parser = parseChar 'a'
    let inputStr = repeatChar times 'a' + "b"
    let input = fromStr inputStr
    let testFn = many1 parser
    // act
    let result = run testFn input
    // assert
    let expectedValues = 
        {1..times}
        |> Seq.map (fun i -> 'a')
        |> Seq.toList
    Assert.Success expectedValues result    

[<Fact>]
let Should_failureBeReturned_when_many1CannotMatchToken () =
    // arrange    
    let parser = failingParser
    let testFn = many1 parser
    // act
    let result = run testFn singleLineInput
    // assert
    let expectedPos = {line=0; column=0; currentLine = exampleLine1}
    Assert.FailureOnPos expectedPos result

// TODO: test many with sep

[<Fact>]
let Should_optReturnNone_when_parserFailed () =
    // arrange    
    let parser = parseChar 'c'
    let inputStr = "ab"
    let input = fromStr inputStr
    let testFn = opt parser
    // act
    let result = run testFn input
    // assert
    let expectedValue = None
    Assert.Success expectedValue result
    
[<Fact>]
let Should_optReturnSomeValue_when_parserSucceed () =
    // arrange    
    let parser = parseChar 'a'
    let inputStr = "ab"
    let input = fromStr inputStr
    let testFn = opt parser
    // act
    let result = run testFn input
    // assert
    let expectedValue = Some 'a'
    Assert.Success expectedValue result
