module Repl

type Arguments = {
    printAst: bool
    printIR: bool
    printTIR: bool
}

let runRepl (args: Arguments) =
    ()