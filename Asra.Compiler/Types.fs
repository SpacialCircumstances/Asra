﻿module Types

type CompilerArguments = {
    inFile: string
    outFile: string
}

type Arguments = {
    formatAst: FrontendAst.Expression<AstCommon.SourcePosition> -> unit
    formatIR: IR.Expression<AstCommon.SourcePosition, AstCommon.Declaration> -> unit
    log: string -> unit
}

[<StructuredFormatDisplay("{AsString}")>]
type CompilerError =
    | IOError of string
    | ParserError of string
    | TypecheckError of Typechecker.TypeError
with
    override self.ToString () = match self with
                                    | IOError e -> e
                                    | ParserError e -> e
                                    | TypecheckError t -> sprintf "%A" t
    member self.AsString = self.ToString ()