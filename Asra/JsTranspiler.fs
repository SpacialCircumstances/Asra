module JsTranspiler

open JsBackend

type NameMapping = string option -> Variable

type Context = {
    nameMapping: NameMapping
    statements: Statement list
}

let getName (ctx: Context ref) (nameHint: string option) = (!ctx).nameMapping nameHint

let addStatement (ctx: Context ref) (st: Statement) = 
    let oldCtx = !ctx
    ctx := { oldCtx with statements = st :: oldCtx.statements }

let finish (ctx: Context ref) (retVal: Variable) = { 
    statements = List.rev (!ctx).statements
    returnValue = retVal
}

let transpile (expr: IR.Expression<Typechecker.DataWithType<'data>>) =
    {
        statements = []
        returnValue = Variable ""
    }