module IR

open AstCommon

[<StructuredFormatDisplay("{AsString}")>]
type LetExpression<'data> = {
    binding: Declaration
    value: Expression<'data>
    body: Expression<'data>
    data: 'data
}
with
    override self.ToString () = sprintf "(Let (%A = %A) In %A @%A)" self.binding self.value self.body self.data
    member self.AsString = self.ToString ()

and [<StructuredFormatDisplay("{AsString}")>] Expression<'data> =
    | Literal of Literal<Expression<'data>> * 'data
    | Variable of string * 'data
    | Application of Expression<'data> * Expression<'data> * 'data
    | Lambda of Declaration * Expression<'data> * 'data
    | Let of LetExpression<'data>
    | Fix of Expression<'data>
    | If of Expression<'data> * Expression<'data> * Expression<'data> * 'data
with
    override self.ToString () = 
        match self with
            | Literal (lit, data) -> sprintf "%A@%A" lit data
            | Variable (name, data) -> sprintf "%s@%A" name data
            | Application (funcExpr, argExpr, data) -> sprintf "(%A %A @%A)" funcExpr argExpr data
            | Lambda (decl, expr, data) -> sprintf "(%A -> %A @%A)" decl expr data
            | Let l -> sprintf "%A" l
            | Fix e -> sprintf "(Fix %A)" e
            | If (condExpr, ifExpr, elseExpr, data) -> sprintf "(If %A Then %A Else %A @%A)" condExpr ifExpr elseExpr data 

    member self.AsString = self.ToString ()

let rec getData (expr: Expression<'data>) =
    match expr with
        | Variable (_, data) -> data
        | Literal (_, data) -> data
        | Application (_, _, data) -> data
        | Lambda (_, _, data) -> data
        | Let l -> l.data
        | Fix e -> getData e
        | If (_, _, _, data) -> data