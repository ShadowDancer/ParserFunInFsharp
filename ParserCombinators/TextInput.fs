module ParserCombinators.TextInput

open System

type ParserPosition = {
    line : int
    column : int
    currentLine : string
}

/// define an initial position
let initialPos line = {line=0; column=0; currentLine=line}

/// increment the column number
let incrCol pos = 
    {pos with column=pos.column + 1}

/// increment the line number and set the column to 0
let incrLine pos newLine= 
    {line=pos.line + 1; column=0; currentLine=newLine}

type InputState =
    {
        lines: string[]
        position:ParserPosition
    }

let fromStr str = 
    if String.IsNullOrEmpty(str) then
        {lines=[|""|]; position=initialPos ""}
    else
        let separators = [| "\r\n"; "\n" |]
        let lines = str.Split(separators, StringSplitOptions.None)
        {lines=lines; position=initialPos lines.[0]}
        
let private currentLine inputState = 
    let linePos = inputState.position.line
    inputState.lines.[linePos]
        
let nextToken input =
    let linePos = input.position.line
    let colPos = input.position.column

    let currentLine = currentLine input
    let inCurrentLine = colPos < currentLine.Length 
    if inCurrentLine then
        let char = currentLine.[colPos]
        let newPos = incrCol input.position 
        let newState = {input with position=newPos}
        newState, Some char
    else 
        let newLine = input.position.line+1
        let isLastLine = newLine >= input.lines.Length
        if isLastLine then
            input, None
        else
            let char = '\n'
            let newPos = incrLine input.position (input.lines.[newLine])
            let newState = {input with position=newPos}
            newState, Some char
        
let parserPositionFromInputState (inputState:InputState) = {
    currentLine =   currentLine inputState
    line = inputState.position.line
    column = inputState.position.column
    }
    