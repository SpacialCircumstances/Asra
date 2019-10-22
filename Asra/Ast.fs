module Ast

type CodePosition = FParsec.Position

type TypeDeclaration =
    | Name of string
    | Generic of string
    | Parameterized of string * TypeDeclaration list
    | Function of TypeDeclaration * TypeDeclaration

type Declaration =
    | Named of string
    | TypeAnnotated of string * TypeDeclaration

type LetBinding = Declaration * Expression

and Expression =
    | Let of LetBinding list * Expression * CodePosition
    | Variable of string * CodePosition
    | Lambda of Declaration list * Expression * CodePosition
    | FunctionCall of Expression * (Expression list) * CodePosition
    | If of Expression * Expression * Expression * CodePosition
    | Import of string * CodePosition