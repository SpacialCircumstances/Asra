module IR

open AstCommon

[<StructuredFormatDisplay("{AsString}")>]
type LetExpression<'data, 'decl> = {
    binding: 'decl
    value: Expression<'data, 'decl>
    body: Expression<'data, 'decl>
    data: 'data
}
with
    override self.ToString () = sprintf "(Let (%A = %A) In %A @%A)" self.binding self.value self.body self.data
    member self.AsString = self.ToString ()

and [<StructuredFormatDisplay("{AsString}")>] Expression<'data, 'decl> =
    | Literal of Literal<Expression<'data, 'decl>> * 'data
    | Variable of string * 'data
    | Application of Expression<'data, 'decl> * Expression<'data, 'decl> * 'data
    | Lambda of 'decl * Expression<'data, 'decl> * 'data
    | Let of LetExpression<'data, 'decl>
    | LetRec of LetExpression<'data, 'decl>
    | If of Expression<'data, 'decl> * Expression<'data, 'decl> * Expression<'data, 'decl> * 'data
with
    override self.ToString () = 
        match self with
            | Literal (lit, data) -> sprintf "%A@%A" lit data
            | Variable (name, data) -> sprintf "%s@%A" name data
            | Application (funcExpr, argExpr, data) -> sprintf "(%A %A @%A)" funcExpr argExpr data
            | Lambda (decl, expr, data) -> sprintf "(%A -> %A @%A)" decl expr data
            | Let l -> sprintf "%A" l
            | LetRec l -> sprintf "%A" l
            | If (condExpr, ifExpr, elseExpr, data) -> sprintf "(If %A Then %A Else %A @%A)" condExpr ifExpr elseExpr data 

    member self.AsString = self.ToString ()

let getData (expr: Expression<'data, 'decl>) =
    match expr with
        | Variable (_, data) -> data
        | Literal (_, data) -> data
        | Application (_, _, data) -> data
        | Lambda (_, _, data) -> data
        | Let l -> l.data
        | LetRec l -> l.data
        | If (_, _, _, data) -> data