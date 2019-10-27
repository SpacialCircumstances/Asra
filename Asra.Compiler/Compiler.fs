module Compiler

open System.IO

type Arguments = {
    file: string
    astPrint: TextWriter
    irPrint: TextWriter
    log: TextWriter
    tirPrint: TextWriter
}

let runCompiler (args: Arguments) =
    Ok ""