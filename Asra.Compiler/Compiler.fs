module Compiler

open System.IO

type Arguments = {
    file: string
    astPrint: TextWriter
    irPrint: TextWriter
    log: TextWriter
    tirPrint: TextWriter
}

let runCompiler (args: Arguments) =
    if File.Exists args.file then
        let code = File.ReadAllText args.file
        match Parser.compilerParser args.file code with
            | Ok ast ->
                args.astPrint.WriteLine (sprintf "%A" ast)
                Ok "Compilation finished"
            | Error parserError ->
                Error (sprintf "Error: %s" parserError)
    else
        Error (sprintf "Error: File %s does not exist" args.file)