module Types

type CompilerArguments = {
    inFile: string
    outFile: string
}

type Arguments = {
    formatAst: FrontendAst.Expression<AstCommon.SourcePosition> -> unit
    formatIR: IR.Expression<AstCommon.SourcePosition, AstCommon.Declaration> -> unit
    formatTypedIR: IR.Expression<Typechecker.TypeData<AstCommon.SourcePosition>, Typechecker.Declaration> -> unit
    formatEquations: Typechecker.TypeEquation<AstCommon.SourcePosition> seq -> unit
    formatSubstitutions: Typechecker.Substitutions -> unit
    log: string -> unit
}