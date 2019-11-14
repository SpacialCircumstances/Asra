﻿module Repl

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
    Errors.result {
        let! ast = Parser.compilerParser "REPL" code
        do args.formatAst ast
        let ir = IRGenerator.map ast
        do args.formatIR ir
        let! typedIR = Typechecker.generateTypenames ir
        do args.formatTypedIR typedIR
        let eqs = Typechecker.generateEquations typedIR
        do args.formatEquations eqs
        let! subst = Typechecker.unifyAll eqs
        do args.formatSubstitutions subst
        let programType = (Typechecker.getType typedIR |> Typechecker.resolveType subst)
        do args.log (sprintf "Program type: %A" programType)
    } |> ignore

let runRepl (args: Arguments) =
    let run = runCode args
    let mutable running = true
    while running do
        let code = readCode ()
        match code.Trim() with
            | ":q" | ":quit" | ":exit" -> running <- false
            | _ -> run code
                
    ()