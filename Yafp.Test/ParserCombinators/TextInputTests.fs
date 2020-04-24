module Yasc.Test.ParserCombinators.TextInputTests

open Xunit
open ParserCombinators.TextInput
open Auxiliary

[<Fact>]
let Should_countersBeZero_in_initialPos () =
    // arrange
    let line = exampleLine1
    // act
    let pos = initialPos line
    // assert
    Assert.Equal(0, pos.column)
    Assert.Equal(0, pos.line)
    Assert.Equal(line, pos.currentLine)
    
[<Fact>]
let Should_columnBeIncremented_by_incrCol () =
    // arrange 
    let pos = initialPos exampleLine1
    let testFn = incrCol
    // act
    let incrPos = testFn pos
    // assert
    Assert.Equal(incrPos.line, 0)
    Assert.Equal(incrPos.column, 1)
    Assert.Equal(incrPos.currentLine, exampleLine1)
    
[<Fact>]
let Should_ColumnBeIncremented_WhenCalled_incColMultipleTimes () =
    // arrange 
    let pos = initialPos exampleLine1
    let testFn = incrCol >> incrCol >> incrCol
    // act
    let incrPos = testFn pos
    // assert
    Assert.Equal(incrPos.line, 0)
    Assert.Equal(incrPos.column, 3)
    Assert.Equal(incrPos.currentLine, exampleLine1)
 
[<Fact>]
let Should_lineBeIncremented_by_incrLine () =
    // arrange 
    let pos = initialPos multilineText
    let testFn = incrLine
    // act
    let line2pos = testFn pos exampleLine2
    // assert
    Assert.Equal(line2pos.line, 1)
    Assert.Equal(line2pos.currentLine, exampleLine2)

[<Fact>]
let Should_columnBeZeroed_by_incrLine () =
    // arrange 
    let pos = 
        initialPos multilineText
        |> incrCol 
        |> incrCol
    let testFn = incrLine
    // act
    let line2pos = testFn pos exampleLine2
    // assert
    Assert.Equal(line2pos.line, 1)
    Assert.Equal(line2pos.column, 0)
    
[<Fact>]
let Should_correctRepresentationByRedurned_by_fromStr () =
    // arrange 
    let testFn = fromStr
    // act
    let inputState = testFn multilineText
    // assert
    let expectedLines = [| exampleLine1; exampleLine2; exampleLine3 |]
    let expectedPosition = initialPos exampleLine1
    Assert.Equal<string[]>(expectedLines, inputState.lines)
    Assert.Equal(expectedPosition, inputState.position)
    
[<Fact>]
let Should_returnNewline_after_readingWholeInput () =
    // arrange 
    let input = fromStr ""
    // act
    let _, result = nextToken input
    // assert
    Assert.Equal(None, result)

[<Fact>]
let Should_newCharacterBeReturned_byNextToken_onSingleLine () =
    // arrange 
    let input = fromStr exampleLine1
    // act
    let _, result = nextToken input
    // assert
    let expectedChar = Some exampleLine1.[0]
    Assert.Equal(expectedChar, result)

    
[<Fact>]
let Should_newLineBeReturned_byNextToken_onEndOfLine () =
    // arrange 
    let input = 
        advanceInput 1 (fromStr "a\nb")
    // act
    let _, result = nextToken input
    // assert
    let expectedChar = Some '\n'
    Assert.Equal(expectedChar, result)

[<Fact>]
let Should_characterFromNextBeReturned_byNextToken_afterNewline () =
    // arrange 
    let input = 
      fromStr "a\nb"
      |> advanceInput 2
    // act
    let _, result = nextToken input
    // assert
    let expectedChar = Some 'b'
    Assert.Equal(expectedChar, result)
