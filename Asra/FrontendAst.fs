module FrontendAst

open AstCommon

[<StructuredFormatDisplay("{AsString}")>]
type BindingModifier = 
    | Recursive
with 
    override self.ToString () =
        match self with
            | Recursive -> "rec"
    member self.AsString = self.ToString ()

type LetBinding<'data> = (BindingModifier option) * Declaration * Expression<'data>

and [<StructuredFormatDisplayAttribute("{AsString}")>] Expression<'data> =
    | Literal of Literal<Expression<'data>> * 'data
    | Group of Expression<'data> * 'data
    | Let of LetBinding<'data> list * Expression<'data> * 'data
    | Variable of string * 'data
    | Lambda of Declaration list * Expression<'data> * 'data
    | FunctionCall of Expression<'data> * (Expression<'data> list) * 'data
    | If of Expression<'data> * Expression<'data> * Expression<'data> * 'data
    | Import of string * 'data
    | UnaryOperatorCall of string * Expression<'data> * 'data
    | BinaryOperatorCall of Expression<'data> * string * Expression<'data> * 'data
with
    override self.ToString () =
        let printBinding (modifier, decl, expr) = 
            match modifier with
                | Some modi -> sprintf "%A %A = %A" modi decl expr
                | None -> sprintf "%A = %A" decl expr

        match self with
            | Literal (lit, data) -> sprintf "(Literal %A @%A)" lit data
            | Group (expr, data) -> sprintf "(Group %A @%A)" expr data
            | BinaryOperatorCall (op1, operator, op2, data) -> sprintf "(BinOp %s %A %A @%A)" operator op1 op2 data
            | FunctionCall (funcExpr, argsExpr, data) -> sprintf "(Call %A [%s] @%A)" funcExpr (System.String.Join("; ", argsExpr)) data
            | If (ifExpr, thenExpr, elseExpr, data) -> sprintf "(If %A then %A else %A @%A)" ifExpr thenExpr elseExpr data
            | Import (name, data) -> sprintf "(Import %s @%A)" name data
            | Lambda (decls, expr, data) -> sprintf "(Lambda [%s] %A @%A)" (System.String.Join(" ", decls)) expr data
            | Let (bindings, expr, data) -> sprintf "(Let [%s] %A @%A)" (System.String.Join("; ", Seq.map printBinding bindings)) expr data
            | UnaryOperatorCall (op, expr, data) -> sprintf "(UnOp %s %A @%A)" op expr data
            | Variable (name, data) -> sprintf "%s@%A" name data

    member self.AsString = self.ToString ()