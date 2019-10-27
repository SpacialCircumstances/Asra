open Argu
open System

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<CLI.Arguments>(programName="asra", errorHandler = errorHandler)
    try 
        let results = parser.Parse(inputs=argv, raiseOnUsage=true)
        CLI.run results
    with
        | e -> 
            printfn "%s" e.Message
            255
