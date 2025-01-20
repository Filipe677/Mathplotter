namespace MathInterpreter

open System
open Types
open Lexer
open Parser

type InterpreterState = {
    SymbolTable: Map<string, float>
}

module Program =
    let initialState = {
        SymbolTable = Map.empty
    }

    let processLine (input: string) (state: InterpreterState) =
        try
            let tokens = Lexer.tokenise input
            let parseState = Parser.initParser tokens
            let (statement, _) = Parser.parseStatement parseState
            let (result, newSymbols) = Parser.evaluate statement state.SymbolTable
            result, { state with SymbolTable = newSymbols }
        with
            | ex -> 
                printfn "Error: %s" ex.Message
                (-1.0, state)

    let rec repl state =
        printf "> "
        let input = Console.ReadLine()
        
        match input.ToLower() with
        | "exit" | "quit" -> 
            printfn "Goodbye!"
            0
        | "vars" ->
            state.SymbolTable 
            |> Map.iter (fun name value -> 
                printfn "%s = %f" name value)
            repl state
        | _ ->
            let (result, newState) = processLine input state
            if result <> -1.0 then
                printfn "Result: %f" result
            repl newState

    let EvaluateExpression (input: string) =
        let state = { SymbolTable = Map.empty }
        let (result, _) = processLine input state
        result

    [<EntryPoint>]
    let main argv =
        printfn "Math Interpreter v1"
        printfn "Type 'exit' or 'quit' to end the session"
        printfn "Type 'vars' to see all defined variables"
        printfn ""
        repl initialState