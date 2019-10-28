module IR

type Literal =
    | Int of int64
    | String of string
    | Float of float
    | Bool of bool
    | Unit

type LetExpression<'data> = {
    varName: string
    value: Expression<'data>
    body: Expression<'data>
    data: 'data
}

and Expression<'data> =
    | Literal of Literal * 'data
    | Variable of string * 'data
    | Application of Expression<'data> * Expression<'data> * 'data
    | Lambda of string * Expression<'data> * 'data
    | Let of LetExpression<'data>
    | LetRec of LetExpression<'data>
    | If of Expression<'data> * Expression<'data> * Expression<'data> * 'data