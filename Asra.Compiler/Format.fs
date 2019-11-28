﻿module Format

open System.IO

type Formatter<'a> = TextWriter -> 'a -> unit

let formatLog: Formatter<string> = fun writer -> fprintfn writer "LOG: %s"

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

let formatIR: Formatter<IR.Expression<AstCommon.SourcePosition, AstCommon.Declaration>> = 
    withHeader "IR" (fun fmt ir -> fmt "%A" ir)

let formatTypedIR: Formatter<IR.Expression<Typechecker.TypeData<AstCommon.SourcePosition>, Typechecker.Declaration>> =
    withHeader "Typed IR" (fun fmt tir -> fmt "%A" tir)

let formatEquations: Formatter<Result<Typechecker.TypeEquation<AstCommon.SourcePosition>, string> seq> =
    withHeader "Type equations" (fun fmt eqs -> 
        Seq.iter (fun eq -> 
            let strEq = match eq with
                        | Ok e -> sprintf "%A" e
                        | Error e -> sprintf "Error: %s" e
            fmt "%s" strEq) eqs)

let formatSubstitutions: Formatter<Typechecker.Substitutions> = 
    withHeader "Type substitutions" (fun fmt subst -> 
        Map.iter (fmt "'%s = %A") subst)