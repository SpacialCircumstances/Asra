module CLI

open Argu
open System
open System.IO

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
                printAst = replArgs.Contains(ReplArgs.PrintAst)
                printIR = replArgs.Contains(ReplArgs.PrintIR)
                printTIR = replArgs.Contains(ReplArgs.PrintTypedIR)
            }
            Repl.runRepl args
            0
        | Some (CompileFile compileArgs) ->
            let createWriter path = 
                match path with
                    | Some None -> System.Console.Out
                    | Some (Some path) ->
                        File.CreateText(path) :> TextWriter
                    | None -> TextWriter.Null

            let args: Compiler.Arguments = {
                file = compileArgs.GetResult(File)
                log = printfn "%s"
                formatAst = printfn "%A"
                formatIR = printfn "%A"
                formatTypedIR = printfn "%A"
                formatEquations = printfn "%A"
                formatSubstitutions = printfn "%A"
            }
            match Compiler.runCompiler args with
                | Ok res -> 
                    printfn "%s" res
                    0
                | Error e -> 
                    printfn "%s" e
                    255