open Argu
open System

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<CLI.Arguments>(programName="asra", errorHandler = errorHandler)
    let results = parser.Parse argv
    CLI.run results
    0
