module IR

open AstCommon

type LetExpression<'data> = {
    binding: Declaration
    value: Expression<'data>
    body: Expression<'data>
    data: 'data
}

and Expression<'data> =
    | Literal of Literal<Expression<'data>> * 'data
    | Variable of string * 'data
    | Application of Expression<'data> * Expression<'data> * 'data
    | Lambda of Declaration * Expression<'data> * 'data
    | Let of LetExpression<'data>
    | LetRec of LetExpression<'data>
    | If of Expression<'data> * Expression<'data> * Expression<'data> * 'data