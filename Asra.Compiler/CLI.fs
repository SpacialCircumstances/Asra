module CLI

open Argu

type CompileArgs =
    | [<MainCommand; ExactlyOnce>] File of file:string
    | [<Mandatory; AltCommandLine("-o")>] Out of outFile:string
    | [<AltCommandLine("-v")>] Verbose
with
    interface IArgParserTemplate with
        member self.Usage =
            match self with
                | Verbose -> "Show Log/AST/IR/TEQ/TIR"
                | Out _ -> "The output js file"
                | File _ -> "The file about to be compiled"

type ReplArgs =
    | [<AltCommandLine("-v")>] Verbose
with
    interface IArgParserTemplate with
        member self.Usage =
            match self with
                | Verbose -> "Show Log/AST/IR/TEQ/TIR"

type Arguments =
    | [<Unique; AltCommandLine("-V")>] Version
    | [<CliPrefix(CliPrefix.None)>] Repl of ParseResults<ReplArgs>
    | [<CliPrefix(CliPrefix.None); AltCommandLine("c")>] Compile of ParseResults<CompileArgs>
with
    interface IArgParserTemplate with
        member self.Usage =
            match self with
                | Version -> "Print the version of Asra and exit"
                | Repl _ -> "Launch the REPL (no interpretation, only Parsing/Type checking)"
                | Compile _ -> "Compile a file to Javascript"