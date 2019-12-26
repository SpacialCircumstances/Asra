module JsTranspiler

open JsBackend
open IR

let transpile (expr: Expression<Typechecker.DataWithType<'data>>) =
    {
        statements = []
        returnValue = ""
    }