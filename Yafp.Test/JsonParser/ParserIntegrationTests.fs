module ParserIntegrationTests

open ParserCombinators.TextInput
open ParserCombinators.Core
open JsonParser.Model
open JsonParser.Parser
open Xunit

[<Fact>]
let Should_parse_emptyArray () =
    // arrange
    let json = "[]"
    // act
    let result = parseString json
    //assety
    Assert.Equal<JValue>(JArray [], result)

[<Fact>]
let Should_parse_null () =
    // arrange
    let json = "null"
    // act
    let result = parseString json
    //assety
    Assert.Equal<JValue>(JNull, result)
    
[<Fact>]
let Should_parse_true () =
    // arrange
    let json = "true"
    // act
    let result = parseString json
    //assety
    Assert.Equal<JValue>(JBool true, result)
        
[<Fact>]
let Should_parse_false () =
    // arrange
    let json = "false"
    // act
    let result = parseString json
    //assety
    Assert.Equal<JValue>(JBool false, result)

[<Fact>]
let Should_parse_object () =
    // arrange
    let json = "{ \"What is meaning of life\" \t :   42   }  "
    // act
    let result = parseString json
    //assety
    let expectedMap = JObject (Map [("What is meaning of life", JNumber 42.0)])
    Assert.Equal<JValue>(expectedMap, result)