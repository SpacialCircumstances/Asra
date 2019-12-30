module JsBackend

type Variable = Variable of string

type Block = {
    statements: Statement list
    returnValue: Value
}

and Value =
    | Var of Variable
    | Literal of AstCommon.Literal<Value>
    | ShortLambda of Variable * Value
    | BlockLambda of Variable * Block
    | FunctionCall of Value * Value

and Statement = 
    | Import
    | Assignment of Variable * Value
    | ShortConditionalAssignment of Variable * Value * Value * Value
    | ConditionalAssignment of Variable * Value * Block * Block
