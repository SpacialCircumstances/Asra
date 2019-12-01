module Compiler

open System.IO
open Types

let fileRead (filename: string) =
    if File.Exists filename then
        File.ReadAllText filename |> Ok
    else
        Error (sprintf "File %s does not exist" filename)

let runCompiler (args: Arguments) (compilerArgs: CompilerArguments) =
    let file = compilerArgs.inFile
    let res = Errors.result {
        let! code = fileRead file |> Result.mapError IOError
        let! ast = Parser.compilerParser file code |> Result.mapError ParserError
        do args.formatAst ast
        let ir = IRGenerator.map ast
        do args.formatIR ir
        let tc = Typechecker.createContext Prelude.context
        let typedIR = tc.generateTypenames ir
        do args.formatTypedIR typedIR
        let eqs = tc.generateEquations typedIR
        do args.formatEquations eqs
        let! subst = tc.solveEquations eqs |> Result.mapError TypecheckError
        do args.formatSubstitutions subst
        let programType = (tc.getExprType typedIR subst)
        do args.log (sprintf "Program type: %A" programType)
        return "Compilation finished"
    }
    match res with
        | Ok m -> 
            args.log m
            Ok ()
        | Error (IOError e) -> 
            args.log (sprintf "IO Error: %s" e)
            Error ()
        | Error (ParserError e) -> 
            args.log (sprintf "Parser Error: %s" e)
            Error ()
        | Error (TypecheckError (e, subst)) ->
            args.log (sprintf "Type Error: %s" e)
            args.formatSubstitutions subst
            Error ()