module Yasc.Test.ParserCombinators.CoreParseLogicalTests

open Xunit
open Xunit.Sdk
open ParserCombinators.Core
open ParserCombinators.TextInput
open ParserCombinators.Characters
open Auxiliary


[<Fact>]
let Should_givenLabelBeReturned_by_satisfy () =
    // arrange 
    let predicate _x = true
    let testFn = satisfy predicate defaultLabel
    // act
    let result = getLabel testFn
    // assert
    Assert.Equal(defaultLabel, result)
    
[<Fact>]
let Should_failureBeRetruned_by_satisfyWhenPredicateFailed () =
    // arrange 
    let notMatchingCharacter = '1'
    let predicate x = (notMatchingCharacter).Equals(x)
    let testFn = satisfy predicate defaultLabel
    // act
    let result = run testFn singleLineInput
    // assert
    Assert.Failure result

[<Fact>]
let Should_inputBeAdvancedBySatisfy_when_predicateMatched () =
    // arrange 
    let inputCharacter = exampleLine1.[0]
    let predicate x = (inputCharacter).Equals(x)
    let testFn = satisfy predicate defaultLabel
    // act
    let result = run testFn singleLineInput
    // assert
    let expectedInput, _ = nextToken singleLineInput
    Assert.SuccessWithInput expectedInput result
    
[<Fact>]
let Should_satisfyReturnParsedCharacterFrom_when_predicateMatched () =
    // arrange 
    let inputCharacter = exampleLine1.[0]
    let predicate x = (inputCharacter).Equals(x)
    let testFn = satisfy predicate defaultLabel
    // act
    let result = run testFn singleLineInput
    // assert
    let _, someResult = nextToken singleLineInput
    let expectedChar = Option.defaultValue '0' someResult
    Assert.Success expectedChar result
    
[<Fact>]
let Should_andThenReturnSuccess_when_bothParsersReturnSuccess () =
    // arrange
    let parser1 = parseChar 'a'
    let parser2 = parseChar 'b'
    let input = fromStr "ab"
    let testFn = andThen parser1 parser2
    // act
    let result = run testFn input
    // assert
    Assert.Success ('a', 'b') result

[<Fact>]
let Should_andThenReturnFailure_when_firstParserFails () =
    // arrange
    let parser1 = parseChar 'c'
    let parser2 = parseChar 'b'
    let inputStr = "ab"
    let input = fromStr inputStr
    let testFn = andThen parser1 parser2
    // act
    let result = run testFn input
    // assert
    let expectedPos = { line=0; column=0; currentLine=inputStr }
    Assert.FailureOnPos expectedPos result

[<Fact>]
let Should_andThenReturnFailure_when_secondParserFails () =
    // arrange
    let parser1 = parseChar 'a'
    let parser2 = parseChar 'c'
    let inputStr = "ab"
    let input = fromStr inputStr
    let testFn = andThen parser1 parser2
    // act
    let result = run testFn input
    // assert
    let expectedPos = { line=0; column=1; currentLine=inputStr }
    Assert.FailureOnPos expectedPos result

[<Fact>]
let Should_orElseReturnSuccess_when_bothParsersReturnSuccess () =
    // arrange
    let parser1 = parseChar 'a'
    let parser2 = parseChar 'a'
    let input = fromStr "ab"
    let testFn = orElse parser1 parser2
    // act
    let result = run testFn input
    // assert
    Assert.Success 'a' result
    
[<Fact>]
let Should_orElseReturnSuccess_when_firstParserReturnsSuccess () =
    // arrange
    let parser1 = parseChar 'a'
    let parser2 = parseChar 'c'
    let input = fromStr "ab"
    let testFn = orElse parser1 parser2
    // act
    let result = run testFn input
    // assert
    Assert.Success 'a' result
    
[<Fact>]
let Should_orElseReturnSuccess_when_secondParserReturnsSuccess () =
    // arrange
    let parser1 = parseChar 'c'
    let parser2 = parseChar 'a'
    let input = fromStr "ab"
    let testFn = orElse parser1 parser2
    // act
    let result = run testFn input
    // assert
    Assert.Success 'a' result

[<Fact>]
let Should_orElseReturnFailure_when_bothParsersFail () =
    // arrange
    let parser1 = parseChar 'c'
    let parser2 = parseChar 'z'
    let inputStr = "ab"
    let input = fromStr inputStr
    let testFn = orElse parser1 parser2
    // act
    let result = run testFn input
    // assert
    let expectedPos = { line=0; column=0; currentLine=inputStr }
    Assert.FailureOnPos expectedPos result

[<Fact>]
let ``Should_resultsFromLeftParserBeReturned_when_using.>>`` () =
    // arragne
    let leftParser = parseChar 'a'
    let rightParser = parseChar 'b'
    let input = fromStr "ab"
    let testFn = leftParser .>> rightParser
    // act
    let result = run testFn input
    // assert
    Assert.Success 'a' result

[<Fact>]
let ``Should_resultsFromRightParserBeReturned_when_using>>.`` () =
    // arragne
    let leftParser = parseChar 'a'
    let rightParser = parseChar 'b'
    let input = fromStr "ab"
    let testFn = leftParser >>. rightParser
    // act
    let result = run testFn input
    // assert
    Assert.Success 'b' result
    
[<Fact>]
let ``Should_resultsFromMidleParserBeReturned_when_usingBetween`` () =
    // arragne
    let leftParser = parseChar 'a'
    let middleParser = parseChar 'b'
    let rightParser = parseChar 'c'
    let input = fromStr "abc"
    let testFn = between leftParser middleParser rightParser
    // act
    let result = run testFn input
    // assert
    Assert.Success 'b' result