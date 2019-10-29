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