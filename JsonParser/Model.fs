module JsonParser.Model

let whitespaceChars = [' '; '\n'; '\r'; '\t']
let escapedChars = ['"'; '\\'; '/'; 'b'; 'f'; 'n'; 'r'; 't']

type JValue = 
    | JString of string
    | JNumber of double
    | JObject of Map<string, JValue>
    | JArray of list<JValue>
    | JBool of bool
    | JNull
    

