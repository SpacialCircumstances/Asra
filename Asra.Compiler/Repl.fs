module Repl

open System

type Arguments = {
    printAst: bool
    printIR: bool
    printTIR: bool
}

let readCode () =
    let mutable reading = true
    let mutable lle = false
    let lines = ResizeArray()
    while reading do
        Console.Write("> ")
        let line: string = Console.ReadLine()
        lines.Add line
        if String.IsNullOrWhiteSpace line then
            if lle then
                reading <- false
            else
                lle <- true
        else
            lle <- false
     
    String.Join("\n", lines)

let runCode (args: Arguments) (code: string) =
    match Parser.compilerParser "REPL" code with
        | Ok ast ->
            if args.printAst then
                printfn "%A" ast
            let ir = IRGenerator.map ast
            if args.printIR then
                printfn "%A" ir
            match Typechecker.generateTypenames ir with
                | Ok typedIR -> 
                    let eqs = Typechecker.generateEquations typedIR
                    let subst = Typechecker.unifyAll eqs
                    if args.printTIR then
                        printfn "%A" typedIR
                        printfn "%A" eqs
                        match subst with
                            | Ok subst ->
                                printfn "%A" subst
                                printfn "Inferred type for program: %A" (Typechecker.getType typedIR |> Typechecker.resolveType subst)
                            | Error se -> printfn "Type Error: %s" se
                | Error te -> printfn "Type Error: %s" te
        | Error pe ->
            printfn "Parser Error: %s" pe

let runRepl (args: Arguments) =
    let run = runCode args
    let mutable running = true
    while running do
        let code = readCode ()
        match code.Trim() with
            | ":q" | ":quit" | ":exit" -> running <- false
            | _ -> run code
                
    ()