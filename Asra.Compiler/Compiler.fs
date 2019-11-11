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
                let ir = IRGenerator.map ast
                args.irPrint.WriteLine (sprintf "%A" ir)
                match Typechecker.generateTypenames ir with
                    | Ok typedIR -> 
                        args.tirPrint.WriteLine (sprintf "%A" typedIR)
                        let eqs = Typechecker.generateEquations typedIR
                        let subst = Typechecker.unifyAll eqs
                        args.tirPrint.WriteLine (sprintf "%A" eqs)
                        args.tirPrint.WriteLine (sprintf "%A" subst)
                        Ok "Compilation finished"
                    | Error ter ->
                        Error (sprintf "Error: %s" ter)
            | Error parserError ->
                Error (sprintf "Error: %s" parserError)
    else
        Error (sprintf "Error: File %s does not exist" args.file)