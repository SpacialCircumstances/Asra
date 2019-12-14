module Types

type CompilerArguments = {
    inFile: string
    outFile: string
}

type Arguments = {
    formatAst: FrontendAst.Expression<AstCommon.SourcePosition> -> unit
    formatIR: IR.Expression<AstCommon.SourcePosition> -> unit
    log: string -> unit
}

[<StructuredFormatDisplay("{AsString}")>]
type CompilerError<'data> =
    | IOError of string
    | ParserError of string
    | TypecheckError of Typechecker.TypeError<'data>
with
    override self.ToString () = match self with
                                    | IOError e -> e
                                    | ParserError e -> e
                                    | TypecheckError t -> sprintf "%A" t
    member self.AsString = self.ToString ()