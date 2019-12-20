module Repl

open System
open Types

let readCode () =
    let mutable reading = true
    let mutable lle = false
    let lines = ResizeArray()
    while reading do
        Console.Write("> ")
        let line: string = Console.ReadLine()
        lines.Add line
        if String.IsNullOrWhiteSpace line then
            if lle then
                reading <- false
            else
                lle <- true
        else
            lle <- false
     
    String.Join("\n", lines)

let runCode (args: Arguments) (code: string) =
    let replResult = Errors.result {
        let! ast = Parser.compilerParser "REPL" code |> Result.mapError ParserError
        do args.formatAst ast
        let ir = IRGenerator.map ast
        do args.formatIR ir
        let tc = Typechecker.createContext Prelude.context args.log
        let! typedIR =  tc ir |> Result.mapError TypecheckError
        return Typechecker.getExprType typedIR
    }

    match replResult with
        | Ok tp -> args.log (sprintf "Program type: %A" tp)
        | Error (IOError e) -> invalidOp (sprintf "Unexpected IO Exception: %s" e)
        | Error (ParserError e) -> args.log (sprintf "Error: %A" e)
        | Error (TypecheckError t) ->
            args.log (sprintf "Type Error: %A" t)

let runRepl (args: Arguments) =
    let run = runCode args
    let mutable running = true
    while running do
        let code = readCode ()
        match code.Trim() with
            | ":q" | ":quit" | ":exit" -> running <- false
            | _ -> run code
                
    ()