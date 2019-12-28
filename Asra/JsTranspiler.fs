module JsTranspiler

open JsBackend

type NameGenerator = string option -> Variable

type NamingMode = Compact | Debug

type Context = {
    nameGen: NameGenerator
    symbolTable: Map<string, string>
    statements: Statement list
}

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