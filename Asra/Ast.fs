module Ast

type TypeDeclaration =
    | Name of string
    | Generic of string
    | Parameterized of string * TypeDeclaration list
    | Function of TypeDeclaration * TypeDeclaration

type Declaration =
    | Named of string
    | TypeAnnotated of string * TypeDeclaration

type LetBinding<'data> = Declaration * Expression<'data>

and Literal =
    | Int of int64
    | String of string
    | Float of float
    | Unit

and Expression<'data> =
    | Literal of Literal * 'data
    | Group of Expression<'data> * 'data
    | Let of LetBinding<'data> list * Expression<'data> * 'data
    | Variable of string * 'data
    | Lambda of Declaration list * Expression<'data> * 'data
    | FunctionCall of Expression<'data> * (Expression<'data> list) * 'data
    | If of Expression<'data> * Expression<'data> * Expression<'data> * 'data
    | Import of string * 'data