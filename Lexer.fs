module Lexer

open Types

let str2lst s = [for c in s -> c]
let isblank c = System.Char.IsWhiteSpace c 
let isdigit c = System.Char.IsDigit c
let isalpha c = System.Char.IsLetter c
let isalnum c = System.Char.IsLetterOrDigit c

type LexerState = {
    Input: string
    Position: int
    Current: char
}

let initState input = {
    Input = input
    Position = 0
    Current = if input.Length > 0 then input.[0] else '\000'
}

let advance state =
    if state.Position >= state.Input.Length - 1 then 
        { state with 
            Position = state.Position + 1
            Current = '\000' }
    else 
        { state with 
            Position = state.Position + 1
            Current = state.Input.[state.Position + 1] }

let peek state =
    if state.Position + 1 >= state.Input.Length then '\000'
    else state.Input.[state.Position + 1]

let rec readWhile state predicate acc =
    if state.Position >= state.Input.Length || not (predicate state.Current) then
        (state, acc)
    else
        readWhile (advance state) predicate (acc + string state.Current)

let readIdentifier state =
    let isIdentifierChar c = isalnum c || c = '_'
    let (newState, identifier) = readWhile state isIdentifierChar ""
    if identifier = "" then
        failwith $"Expected identifier at position {state.Position}"
    (newState, {
        Type = Identifier
        Value = identifier
        Position = state.Position - identifier.Length
    })

let rec readNumber state (acc: string) =
    match state.Current with
    | c when isdigit c -> 
        readNumber (advance state) (acc + string c)
    | '.' when not (acc.Contains(".")) -> 
        if acc = "" && not (isdigit (advance state).Current) then
            failwith $"Invalid number format at position {state.Position}: lone decimal point"
        readNumber (advance state) (acc + ".")
    | c when isalpha c -> 
        failwith $"Invalid character '{c}' in number at position {state.Position}"
    | _ when acc = "" -> 
        failwith $"Expected a number but found '{state.Current}' at position {state.Position}"
    | _ ->
        let token = {
            Type = Number
            Value = acc
            Position = state.Position - acc.Length
        }
        (state, token)

let everyCharacter state =
    let token = 
        match state.Current with
        | '+' -> { Type = Plus; Value = "+"; Position = state.Position }
        | '-' -> { Type = Minus; Value = "-"; Position = state.Position }
        | '*' -> { Type = Multiply; Value = "*"; Position = state.Position }
        | '/' -> { Type = Divide; Value = "/"; Position = state.Position }
        | '(' -> { Type = LeftParen; Value = "("; Position = state.Position }
        | ')' -> { Type = RightParen; Value = ")"; Position = state.Position }
        | '^' -> { Type = Power; Value = "^"; Position = state.Position }
        | '%' -> { Type = Modulo; Value = "%"; Position = state.Position }
        | '.' -> { Type = Dot; Value = "."; Position = state.Position }
        | '=' -> { Type = Equals; Value = "="; Position = state.Position }
        | ';' -> { Type = Semicolon; Value = ";"; Position = state.Position }
        | '\000' -> { Type = EOI; Value = ""; Position = state.Position }
        | c when isalpha c || c = '_' -> 
            let (newState, token) = readIdentifier state
            token
        | _ -> failwith $"Unexpected character '{state.Current}' at position {state.Position}"
    (advance state, token)

let rec tokeniser state tokens =
    if state.Position >= state.Input.Length then
        List.rev ({ Type = EOI; Value = ""; Position = state.Position } :: tokens)
    else
        match state.Current with
        | c when isdigit c ->
            let (newState, token) = readNumber state ""
            tokeniser newState (token :: tokens)
        | c when isblank c ->
            let newState = advance state
            tokeniser newState tokens
        | _ ->
            let (newState, token) = everyCharacter state
            tokeniser newState (token :: tokens)

let tokenise input = 
    let initialState = initState input
    tokeniser initialState []