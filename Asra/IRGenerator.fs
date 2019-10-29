module IRGenerator

let rec map (expr: FrontendAst.Expression<'data>): IR.Expression<'data> =
    invalidOp ""