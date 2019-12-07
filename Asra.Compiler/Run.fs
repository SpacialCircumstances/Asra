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
            let verbose = replArgs.Contains(CLI.ReplArgs.Verbose)
            let out = System.Console.Out
            let nulls = System.IO.TextWriter.Null
            let args = {
                log = Format.formatLog out
                formatAst = Format.formatAst (if verbose then out else nulls)
                formatIR = Format.formatIR (if verbose then out else nulls)
            }
            Repl.runRepl args
            0
        | Some (CLI.Compile compileArgs) ->
            let verbose = compileArgs.Contains(CLI.CompileArgs.Verbose)
            let out = System.Console.Out
            let nulls = System.IO.TextWriter.Null
            let args = {
                log = Format.formatLog out
                formatAst = Format.formatAst (if verbose then out else nulls)
                formatIR = Format.formatIR (if verbose then out else nulls)
            }

            let compilerArgs = {
                inFile = compileArgs.GetResult(CLI.File)
                outFile = compileArgs.GetResult(CLI.Out)
            }
            match Compiler.runCompiler args compilerArgs with
                | Ok _ -> 0
                | Error _ -> 255