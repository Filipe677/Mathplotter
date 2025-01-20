module Parser

open Types
open System

// AST node types
type Expression =
    | Number of float
    | Variable of string
    | UnaryMinus of Expression
    | BinaryOperation of Expression * TokenType * Expression

type Statement =
    | ExpressionStmt of Expression
    | Assignment of string * Expression

type ParserState = {
    Tokens: Token list
    Position: int
    SymbolTable: Map<string, float>
}

let initParser tokens = {
    Tokens = tokens
    Position = 0
    SymbolTable = Map.empty
}

// Helper functions
let current state = 
    if state.Position >= List.length state.Tokens then
        { Type = EOI; Value = ""; Position = -1 }
    else
        state.Tokens.[state.Position]

let advance state = 
    { state with 
        Position = state.Position + 1;
        Tokens = state.Tokens;
        SymbolTable = state.SymbolTable 
    }

let expect tokenType state =
    if (current state).Type = tokenType then
        advance state
    else
        failwith $"Expected {tokenType} at position {(current state).Position}, but got {(current state).Type}"

// Recursive descent parser functions
let rec parseStatement state =
    match (current state).Type with
    | TokenType.Identifier ->
        let identifier = (current state).Value
        let nextState = advance state
        match (current nextState).Type with
        | TokenType.Equals ->
            let afterEquals = advance nextState
            let (expr, exprState) = parseExpression afterEquals
            let finalState = expect Semicolon exprState
            Assignment(identifier, expr), finalState
        | _ -> 
            let (expr, exprState) = parseExpression state
            ExpressionStmt(expr), exprState
    | _ -> 
        let (expr, exprState) = parseExpression state
        ExpressionStmt(expr), exprState

and parseExpression state = 
    parseAdditive state

and parseAdditive state =
    let (left, leftState) = parseMultiplicative state
    let rec loop left currentState =
        match (current currentState).Type with
        | Plus | Minus ->
            let op = (current currentState).Type
            let afterOp = advance currentState
            let (right, rightState) = parseMultiplicative afterOp
            loop (BinaryOperation(left, op, right)) rightState
        | _ -> left, currentState
    loop left leftState

and parseMultiplicative state =
    let (left, leftState) = parsePower state
    let rec loop left currentState =
        match (current currentState).Type with
        | Multiply | Divide | Modulo ->
            let op = (current currentState).Type
            let afterOp = advance currentState
            let (right, rightState) = parsePower afterOp
            loop (BinaryOperation(left, op, right)) rightState
        | _ -> left, currentState
    loop left leftState

and parsePower state =
    let (left, leftState) = parseUnary state
    match (current leftState).Type with
    | Power ->
        let afterPower = advance leftState
        let (right, rightState) = parsePower afterPower
        BinaryOperation(left, Power, right), rightState
    | _ -> left, leftState

and parseUnary state =
    match (current state).Type with
    | Minus ->
        let afterMinus = advance state
        let (expr, exprState) = parsePrimary afterMinus
        UnaryMinus(expr), exprState
    | _ -> parsePrimary state

and parsePrimary state =
    match (current state).Type with
    | TokenType.Number -> 
        Expression.Number(float (current state).Value), advance state
    | TokenType.Identifier ->
        Variable((current state).Value), advance state
    | LeftParen ->
        let afterLeft = advance state
        let (expr, exprState) = parseExpression afterLeft
        let afterExpr = expect RightParen exprState
        expr, afterExpr
    | _ -> failwith $"Unexpected token: {(current state).Type} at position {(current state).Position}"

// Expression evaluator
let rec evaluateExpr (expr: Expression) (symbols: Map<string, float>) =
    match expr with
    | Number value -> value
    | Variable name ->
        match Map.tryFind name symbols with
        | Some value -> value
        | None -> failwith $"Undefined variable '{name}'"
    | UnaryMinus expr -> -(evaluateExpr expr symbols)
    | BinaryOperation (left, operator, right) ->
        let leftValue = evaluateExpr left symbols
        let rightValue = evaluateExpr right symbols
        match operator with
        | Plus -> leftValue + rightValue
        | Minus -> leftValue - rightValue
        | Multiply -> leftValue * rightValue
        | Divide ->
            if rightValue = 0.0 then
                failwith "Division by zero"
            else
                leftValue / rightValue
        | Power -> System.Math.Pow(leftValue, rightValue)
        | Modulo -> 
            if rightValue = 0.0 then
                failwith "Modulo by zero"
            else
                leftValue % rightValue
        | _ -> failwith $"Unexpected operator: {operator}"

// Main evaluation function
let evaluate (statement: Statement) (symbols: Map<string, float>) =
    match statement with
    | ExpressionStmt expr -> 
        evaluateExpr expr symbols, symbols
    | Assignment (name, expr) ->
        let value = evaluateExpr expr symbols
        value, Map.add name value symbols

// Main parse function
let parse input =
    let tokens = Lexer.tokenise input
    let state = initParser tokens
    parseStatement state