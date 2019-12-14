module Format

open System.IO

type Formatter<'a> = TextWriter -> 'a -> unit

let formatLog: Formatter<string> = fun writer -> fprintfn writer "%s"

let withHeader (header: string) (print: (Printf.TextWriterFormat<'a> -> 'a) -> 'b -> unit) =
    fun writer a ->
        let headerWidth = System.Console.WindowWidth
        let h1 = String.init (headerWidth - (String.length header) - 8) (fun _ -> "#")
        let h2 = String.init headerWidth (fun _ -> "#")
        fprintfn writer "##### %s: %s" header h1
        print (fprintfn writer) a
        fprintfn writer "%s" h2

let formatAst: Formatter<FrontendAst.Expression<AstCommon.SourcePosition>> = 
    withHeader "AST" (fun fmt ast -> fmt "%A" ast)

let formatIR: Formatter<IR.Expression<AstCommon.SourcePosition>> = 
    withHeader "IR" (fun fmt ir -> fmt "%A" ir)