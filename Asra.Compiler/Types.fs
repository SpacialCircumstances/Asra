module Types

type CompilerArguments = {
    inFile: string
    outFile: string
}

type Arguments = {
    formatAst: FrontendAst.Expression<AstCommon.SourcePosition> -> unit
    formatIR: IR.Expression<AstCommon.SourcePosition, AstCommon.Declaration> -> unit
    formatTypedIR: IR.Expression<Typechecker.TypeData<AstCommon.SourcePosition>, Typechecker.Declaration> -> unit
    formatEquations: Result<Typechecker.TypeEquation<AstCommon.SourcePosition>, string> seq -> unit
    formatSubstitutions: Typechecker.Substitutions -> unit
    log: string -> unit
}

[<StructuredFormatDisplay("{AsString}")>]
type CompilerError =
    | IOError of string
    | ParserError of string
    | TypecheckError of string * Typechecker.Substitutions
with
    override self.ToString () = match self with
                                    | IOError e -> e
                                    | ParserError e -> e
                                    | TypecheckError (e, s) -> sprintf "%s, %A" e s
    member self.AsString = self.ToString ()