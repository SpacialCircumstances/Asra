module Format

open System.IO

type Formatter<'a> = TextWriter -> 'a -> unit

let formatLog: Formatter<string> = fun writer -> fprintfn writer "LOG: %s"

let withHeader (header: string) (print: (Printf.TextWriterFormat<'a> -> 'a) -> 'b -> unit) =
    fun writer a ->
        fprintfn writer "%s ###" header
        print (fprintfn writer) a
        fprintfn writer "#######"

let formatAst: Formatter<FrontendAst.Expression<AstCommon.SourcePosition>> = 
    withHeader "AST" (fun fmt ast -> fmt "%A" ast)

let formatIR: Formatter<IR.Expression<AstCommon.SourcePosition, AstCommon.Declaration>> = 
    withHeader "IR" (fun fmt ir -> fmt "%A" ir)


let formatTypedIR: Formatter<IR.Expression<Typechecker.TypeData<AstCommon.SourcePosition>, Typechecker.Declaration>> =
    withHeader "Typed IR" (fun fmt tir -> fmt "%A" tir)

let formatEquations: Formatter<Typechecker.TypeEquation<AstCommon.SourcePosition> seq> =
    withHeader "Type equations" (fun fmt eqs -> fmt "%A" eqs)

let formatSubstitutions: Formatter<Typechecker.Substitutions> = 
    withHeader "Type substitutions" (fun fmt subst -> fmt "%A" subst)