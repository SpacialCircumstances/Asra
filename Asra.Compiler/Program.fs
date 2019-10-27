open Argu

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CLI.Arguments>(programName="asra.exe")
    let usage = parser.PrintUsage()
    printfn "%s" usage
    0
