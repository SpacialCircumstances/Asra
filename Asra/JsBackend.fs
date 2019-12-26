module JsBackend

type Variable = string

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
    | ConditionalAssignment
