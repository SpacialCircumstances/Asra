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
    Errors.result {
        let! code = fileRead file
        let! ast = Parser.compilerParser file code
        do args.formatAst ast
        let ir = IRGenerator.map ast
        do args.formatIR ir
        let tc = Typechecker.createContext Prelude.context
        let typedIR = tc.generateTypenames ir
        do args.formatTypedIR typedIR
        let eqs = tc.generateEquations typedIR
        do args.formatEquations eqs
        let! subst = tc.solveEquations eqs
        do args.formatSubstitutions subst
        let programType = (tc.getExprType typedIR subst)
        do args.log (sprintf "Program type: %A" programType)
        return "Compilation finished"
    }