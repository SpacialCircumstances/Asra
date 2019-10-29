module IRGenerator

open AstCommon

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

    match expr with
        | FrontendAst.Literal (lit, data) -> IR.Literal (mapLit lit, data)
        | FrontendAst.Group (expr, _) -> map expr
        | FrontendAst.Variable (name, data) -> IR.Variable (name, data)
        | _ -> invalidOp "Not implemented"