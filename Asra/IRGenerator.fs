module IRGenerator

open AstCommon

let rec lambdaCurry (decls: Declaration list) (expr: IR.Expression<'data>) (data: 'data) =
    match decls with
        | [lastDecl] -> IR.Lambda(lastDecl, expr, data)
        | d :: tail -> IR.Lambda(d, lambdaCurry tail expr data, data)
        | _ -> invalidOp "Lambda cannot have empty parameter list"

let rec map (expr: FrontendAst.Expression<'data>): IR.Expression<'data> =
    let mapLit (lit: Literal<FrontendAst.Expression<'data>>) =
        match lit with
            | List exprs ->
                List (List.map map exprs)
            | Unit -> Unit
            | Int i -> Int i
            | String s -> String s
            | Float f -> Float f
            | Bool b -> Bool b

    let rec expandFunctionCalls (funExpr: IR.Expression<'data>) (args: FrontendAst.Expression<'data> list) (data: 'data) =
        match args with
            | [lastArg] -> IR.Application (funExpr, map lastArg, data)
            | arg :: tail ->
                let nextFun = IR.Application (funExpr, map arg, data)
                expandFunctionCalls nextFun tail data
            | _ -> invalidOp "Function calls must always have at least one argument"

    let rec expandLet (binds: FrontendAst.LetBinding<'data> list) (irExpr: IR.Expression<'data>) (data: 'data) = 
        let (modifier, decl, valueExpr) = List.head binds
        let next = match List.tail binds with
                    | [] -> irExpr
                    | tail -> expandLet tail irExpr data
        let irValueExpr = map valueExpr
        match modifier with
            | None -> 
                IR.Let {
                    data = data
                    binding = decl
                    value = irValueExpr
                    body = next
                }
            | Some FrontendAst.Recursive ->
                IR.LetRec {
                    data = data
                    binding = decl
                    value = irValueExpr
                    body = next
                }


    match expr with
        | FrontendAst.Literal (lit, data) -> IR.Literal (mapLit lit, data)
        | FrontendAst.Group (expr, _) -> map expr
        | FrontendAst.Variable (name, data) -> IR.Variable (name, data)
        | FrontendAst.UnaryOperatorCall (op, expr, data) -> 
            map (FrontendAst.FunctionCall (FrontendAst.Variable (op, data), [ expr ], data))
        | FrontendAst.BinaryOperatorCall (e1, op, e2, data) ->
            map (FrontendAst.FunctionCall (FrontendAst.Variable (op, data), [ e1; e2 ], data))
        | FrontendAst.If (condExpr, ifExpr, elseExpr, data) ->
            IR.If (map condExpr, map ifExpr, map elseExpr, data)
        | FrontendAst.Lambda (decls, expr, data) ->
            let irExpr = map expr
            lambdaCurry decls irExpr data
        | FrontendAst.Import (import, data) -> invalidOp "Imports are not supported at the moment"
        | FrontendAst.Let (bindings, expr, data) ->
            expandLet bindings (map expr) data
        | FrontendAst.FunctionCall (funExpr, args, data) ->
            let irFunExpr = map funExpr
            expandFunctionCalls irFunExpr args data