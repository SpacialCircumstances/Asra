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

//Get js variable from asra name
let getVariable (ctx: Context) (name: string) = 
    Map.tryFind name ctx.symbolTable 
        |> Option.map (fun key -> Map.find key !ctx.jsNameMapping)
        |> Option.map Variable

let addStatement (ctx: Context) (st: Statement) = 
    { ctx with statements = st :: ctx.statements }

let finish (ctx: Context) (retVal: Value) = { 
    statements = List.rev ctx.statements
    returnValue = retVal
}

let rec transpileExpr (expr: IR.Expression<Typechecker.DataWithType<'data>>) (ctx: Context): Context * Value =
    let swap (a, b) = (b, a)
    match expr with
        | IR.Literal (lit, _) ->
            let (newLit, newCtx) = AstCommon.foldLiteral (fun c e -> transpileExpr e c |> swap) ctx lit
            newCtx, Literal newLit

let transpile (expr: IR.Expression<Typechecker.DataWithType<'data>>) (strategy: NamingStrategy) =
    let initialContext = {
        varNameCounter = ref 0
        namingStrategy = strategy
        symbolTable = Map.empty //TODO: Add prelude
        jsNameMapping = ref Map.empty //TODO: Add prelude
        statements = [] //TODO: Import stdlib
    }
    let finalContext, resultValue = transpileExpr expr initialContext
    finish finalContext resultValue