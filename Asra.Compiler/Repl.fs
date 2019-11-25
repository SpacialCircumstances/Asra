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
        let! ast = Parser.compilerParser "REPL" code
        do args.formatAst ast
        let ir = IRGenerator.map ast
        do args.formatIR ir
        let tc = Typechecker.createContext ()
        let! typedIR = tc.generateTypenames Prelude.context ir
        do args.formatTypedIR typedIR
        let eqs = tc.generateEquations typedIR
        do args.formatEquations eqs
        let! subst = tc.solveEquations eqs
        do args.formatSubstitutions subst
        return (tc.getExprType typedIR subst)
    }

    match replResult with
        | Ok tp -> args.log (sprintf "Program type: %A" tp)
        | Error e -> args.log (sprintf "Error: %s" e)

let runRepl (args: Arguments) =
    let run = runCode args
    let mutable running = true
    while running do
        let code = readCode ()
        match code.Trim() with
            | ":q" | ":quit" | ":exit" -> running <- false
            | _ -> run code
                
    ()