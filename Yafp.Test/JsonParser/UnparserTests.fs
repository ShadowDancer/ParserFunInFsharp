module UnparserUnitTests

open ParserCombinators.TextInput
open ParserCombinators.Core
open ParserCombinators.Characters
open JsonParser.Model
open JsonParser.Parser
open JsonParser.Unparser
open Xunit
open Xunit.Abstractions


[<Fact>]
let ParseKvp () = 

    let unpackString x =
        match x with
        | JString v -> v
        | _ -> ""
    let input = fromStr "\"a\":b"
    let pKey = pWhitespaces >>. (pString |>>  unpackString) .>> pWhitespaces
    let parseTest = pWhitespaces
    let pKvp = pKey .>> parseChar ':' .>>. pArray .>> (parseTest
                    |>> fun x -> x)
    
    let json = """"key":[]"""

    let result = runParser pKvp (fromStr json)
    //assety
    Assert.Equal(("key", JArray [] ), result)

[<Fact>]
let Should_parse_escapedString () =
    // arrange
    let json = "\"te\t\n\rst\""
    // act
    let result = runParser pString (fromStr json)
    //assety
    Assert.Equal<JValue>(JString "te\t\n\rst", result)

[<Fact>]
let Should_parse_string () =
    // arrange
    let json = "\"test\""
    // act
    let result = runParser pString (fromStr json)
    //assety
    Assert.Equal<JValue>(JString "test", result)

[<Fact>]
let Should_parse_emptyObject () =
    // arrange
    let json = "{ }"
    // act
    let result = runParser pObject (fromStr json)
    //assety
    Assert.Equal<JValue>(JObject Map.empty, result)
    
[<Fact>]
let Should_parse_objectWithKvp () =
    // arrange
    let json = """{"key": []}"""
    // act
    let result = runParser pObject (fromStr json)
    //assety
    let expectedObj = Map<string, JValue> [("key", JArray [])]
    Assert.Equal<JValue>(JObject expectedObj, result)
    
[<Fact>]
let Should_parse_objectWithManyKvps () =
    // arrange
    let json = """{"key": [], "bool":true, "null": null}"""
    // act
    let result = runParser pObject (fromStr json)
    //assety
    let expectedObj = Map<string, JValue> [("key", JArray []);("bool", JBool true);("null", JNull)]
    Assert.Equal<JValue>(JObject expectedObj, result)
    
[<Fact>]
let Should_parse_number () =
    // arrange
    let json = "1234"
    // act
    let result = runParser pNumber (fromStr json)
    //assety
    Assert.Equal<JValue>(JNumber 1234.0, result)
        
[<Fact>]
let Should_parse_decimalNumber () =
    // arrange
    let json = "42.24"
    // act
    let result = runParser pNumber (fromStr json)
    //assety
    Assert.Equal<JValue>(JNumber 42.24, result)
    
[<Fact>]
let Should_parse_numberInScientificNotation () =
    // arrange
    let json = "42e3"
    // act
    let result = runParser pNumber (fromStr json)
    //assety
    Assert.Equal<JValue>(JNumber 42e3, result)
    
[<Fact>]
let Should_parse_numberInScientificNotationWithMinus () =
    // arrange
    let json = "42e-3"
    // act
    let result = runParser pNumber (fromStr json)
    //assety
    Assert.Equal<JValue>(JNumber 42e-3, result)
    
[<Fact>]
let Should_parse_numberInScientificNotationWithPlus () =
    // arrange
    let json = "42e+3"
    // act
    let result = runParser pNumber (fromStr json)
    //assety
    Assert.Equal<JValue>(JNumber 42e3, result)
    
[<Fact>]
let Should_parse_numberInScientificNotationWithBigE () =
    // arrange
    let json = "42E+3"
    // act
    let result = runParser pNumber (fromStr json)
    //assety
    Assert.Equal<JValue>(JNumber 42e3, result)
        
[<Fact>]
let Should_parse_emptyArray () =
    // arrange
    let json = "[]"
    // act
    let result = runParser pArray (fromStr json)
    //assety
    Assert.Equal<JValue>(JArray [], result)
    
[<Fact>]
let Should_parse_arrayWithObjects () =
    // arrange
    let json = "[true, false, null]"
    // act
    let result = runParser pArray (fromStr json)
    //assety
    Assert.Equal<JValue>(JArray [JBool true; JBool false; JNull], result)
    
[<Fact>]
let Should_parse_null () =
    // arrange
    let json = "null"
    // act
    let result = runParser pNull (fromStr json)
    //assety
    Assert.Equal<JValue>(JNull, result)
    
[<Fact>]
let Should_parse_true () =
    // arrange
    let json = "true"
    // act
    let result = runParser pBool (fromStr json)
    //assety
    Assert.Equal<JValue>(JBool true, result)
    
[<Fact>]
let Should_parse_false () =
    // arrange
    let json = "false"
    // act
    let result = runParser pBool (fromStr json)
    //assety
    Assert.Equal<JValue>(JBool false, result)
        
