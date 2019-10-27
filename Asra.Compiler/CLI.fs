﻿module CLI

open Argu

type CompileArgs =
    | [<MainCommand; ExactlyOnce; Last>] File of file:string
    | [<EqualsAssignment>] PrintAst of path:string option
    | [<EqualsAssignment>] PrintIR of path:string option
    | [<EqualsAssignment>] PrintTypedIR of path:string option
with
    interface IArgParserTemplate with
        member self.Usage =
            match self with
                | PrintAst _ -> "Print the AST after parsing, optionally to a file"
                | PrintIR _ -> "Print the untyped IR, optionally to a file"
                | PrintTypedIR _ -> "Print the typed IR, optionally to a file"
                | File _ -> "Compile this file"

type ReplArgs =
    | PrintAst
    | PrintIR
    | PrintTypedIR
with
    interface IArgParserTemplate with
        member self.Usage =
            match self with
                | PrintAst -> "Print the AST after parsing"
                | PrintIR -> "Print the untyped IR"
                | PrintTypedIR -> "Print the typed IR"

type Arguments =
    | [<Unique; AltCommandLine("-v")>] Version
    | [<CliPrefix(CliPrefix.None)>] Repl of ParseResults<ReplArgs>
    | [<CliPrefix(CliPrefix.None); AltCommandLine("c")>] CompileFile of ParseResults<CompileArgs>
with
    interface IArgParserTemplate with
        member self.Usage =
            match self with
                | Version -> "Print the version of Asra and exit"
                | Repl _ -> "Launch the REPL (no interpretation, only AST/IR/TIR)"
                | CompileFile _ -> "Compile a file to Javascript"

let run (args: ParseResults<Arguments>) =
    let results = args.GetAllResults()
    match List.tryExactlyOne results with
        | None ->
            args.Raise("Wrong number of arguments supplied", errorCode = ErrorCode.CommandLine, showUsage=true)
            255
        | Some Version ->
            printfn "asra %O" Info.compilerVersion
            0
        | Some (Repl replArgs) ->
            let args: Repl.Arguments = {
                printAst = replArgs.Contains(PrintAst)
                printIR = replArgs.Contains(PrintIR)
                printTIR = replArgs.Contains(PrintTypedIR)
            }
            Repl.runRepl args
            0
        | Some (CompileFile compileArgs) ->
            let args: Compiler.Arguments = {
                file = compileArgs.GetResult(File)
            }
            match Compiler.runCompiler args with
                | Ok res -> 
                    printfn "%A" res
                    0
                | Error e -> 
                    printfn "%s" e
                    255