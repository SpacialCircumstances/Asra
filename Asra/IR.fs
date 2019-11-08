module IR

open AstCommon

type LetExpression<'data, 'decl> = {
    binding: 'decl
    value: Expression<'data, 'decl>
    body: Expression<'data, 'decl>
    data: 'data
}

and Expression<'data, 'decl> =
    | Literal of Literal<Expression<'data, 'decl>> * 'data
    | Variable of string * 'data
    | Application of Expression<'data, 'decl> * Expression<'data, 'decl> * 'data
    | Lambda of 'decl * Expression<'data, 'decl> * 'data
    | Let of LetExpression<'data, 'decl>
    | LetRec of LetExpression<'data, 'decl>
    | If of Expression<'data, 'decl> * Expression<'data, 'decl> * Expression<'data, 'decl> * 'data

let getData (expr: Expression<'data, 'decl>) =
    match expr with
        | Variable (_, data) -> data
        | Literal (_, data) -> data
        | Application (_, _, data) -> data
        | Lambda (_, _, data) -> data
        | Let l -> l.data
        | LetRec l -> l.data
        | If (_, _, _, data) -> data