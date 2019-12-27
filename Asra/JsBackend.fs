module JsBackend

type Variable = Variable of string

type Block = {
    statements: Statement list
    returnValue: Variable
}

and Value =
    | Literal of AstCommon.Literal<Value>
    | Lambda of Variable list * Block
    | FunctionCall of Variable * Value list

and Statement = 
    | Import
    | Assignment of Variable * Value
    | ShortConditionalAssignment of Variable * Value * Value * Value
    | ConditionalAssignment of Variable * Value * Block * Block
