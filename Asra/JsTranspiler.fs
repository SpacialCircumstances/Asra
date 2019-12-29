module JsTranspiler

open JsBackend

type NamingStrategy = Compact | Debug

type VariableKey = VarKey of int

type Context = {
    varNameCounter: int ref
    namingStrategy: NamingStrategy
    symbolTable: Map<string, VariableKey>
    jsNameMapping: Map<VariableKey, string> ref
    statements: Statement list
}

let getVariable (ctx: Context) (name: string) = 
    Map.tryFind name ctx.symbolTable |> Option.map (fun key -> Map.find key !ctx.jsNameMapping)

let addStatement (ctx: Context) (st: Statement) = 
    { ctx with statements = st :: ctx.statements }

let finish (ctx: Context) (retVal: Variable) = { 
    statements = List.rev ctx.statements
    returnValue = retVal
}

let transpile (expr: IR.Expression<Typechecker.DataWithType<'data>>) =
    {
        statements = []
        returnValue = Variable ""
    }