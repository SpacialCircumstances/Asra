module Format

open System.IO

type Formatter<'a> = TextWriter -> 'a -> unit

let format (fmt: Printf.TextWriterFormat<'a>) (writer: TextWriter) = fprintf writer fmt

let formatLog: Formatter<string> = format "LOG: %s"

let formatAst: Formatter<FrontendAst.Expression<AstCommon.SourcePosition>> = format "%A"

let formatIR: Formatter<IR.Expression<AstCommon.SourcePosition, AstCommon.Declaration>> = format "%A"

let formatTypedIR: Formatter<IR.Expression<Typechecker.TypeData<AstCommon.SourcePosition>, Typechecker.Declaration>> = format "%A"

let formatEquations: Formatter<Typechecker.TypeEquation<AstCommon.SourcePosition> seq> = format "%A"

let formatSubstitutions: Formatter<Typechecker.Substitutions> = format "%A"