module Run

open System.IO
open Argu
open Types

let run (args: ParseResults<CLI.Arguments>) =
    let results = args.GetAllResults()
    match List.tryExactlyOne results with
        | None ->
            args.Raise("Wrong number of arguments supplied", errorCode = ErrorCode.CommandLine, showUsage=true)
            255
        | Some CLI.Version ->
            printfn "asra %O" Info.compilerVersion
            0
        | Some (CLI.Repl replArgs) ->
            let args = {
                log = printfn "%s"
                formatAst = printfn "%A"
                formatIR = printfn "%A"
                formatTypedIR = printfn "%A"
                formatEquations = printfn "%A"
                formatSubstitutions = printfn "%A"
            }
            Repl.runRepl args
            0
        | Some (CLI.CompileFile compileArgs) ->
            let createWriter path = 
                match path with
                    | Some None -> System.Console.Out
                    | Some (Some path) ->
                        File.CreateText(path) :> TextWriter
                    | None -> TextWriter.Null

            let compilerArgs = {
                inFile = compileArgs.GetResult(CLI.File)
                outFile = ""
            }

            let args = {
                log = printfn "%s"
                formatAst = printfn "%A"
                formatIR = printfn "%A"
                formatTypedIR = printfn "%A"
                formatEquations = printfn "%A"
                formatSubstitutions = printfn "%A"
            }
            match Compiler.runCompiler args compilerArgs with
                | Ok res -> 
                    printfn "%s" res
                    0
                | Error e -> 
                    printfn "%s" e
                    255