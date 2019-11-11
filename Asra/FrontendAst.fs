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

and Expression<'data> =
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