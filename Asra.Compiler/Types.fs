module Types

type Arguments = {
    file: string
    formatAst: FrontendAst.Expression<AstCommon.SourcePosition> -> unit
    formatIR: IR.Expression<AstCommon.SourcePosition, AstCommon.Declaration> -> unit
    formatTypedIR: IR.Expression<Typechecker.TypeData<AstCommon.SourcePosition>, Typechecker.Declaration> -> unit
    formatEquations: Typechecker.TypeEquation<AstCommon.SourcePosition> seq -> unit
    formatSubstitutions: Typechecker.Substitutions -> unit
    log: string -> unit
}