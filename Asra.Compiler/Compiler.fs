module Compiler

open System.IO

type Arguments = {
    file: string
    formatAst: FrontendAst.Expression<AstCommon.SourcePosition> -> unit
    formatIR: IR.Expression<AstCommon.SourcePosition, AstCommon.Declaration> -> unit
    formatTypedIR: IR.Expression<Typechecker.TypeData<AstCommon.SourcePosition>, Typechecker.Declaration> -> unit
    formatEquations: Typechecker.TypeEquation<AstCommon.SourcePosition> seq -> unit
    formatSubstitutions: Typechecker.Substitutions -> unit
    log: string -> unit
}

let fileRead (filename: string) =
    if File.Exists filename then
        File.ReadAllText filename |> Ok
    else
        Error (sprintf "File %s does not exist" filename)

let print _ = ()

let runCompiler (args: Arguments) =
    Errors.result {
        let! code = fileRead args.file
        let! ast = Parser.compilerParser args.file code
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
        return "Compilation finished"
    }