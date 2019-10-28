module AstCommon

type Literal<'expr> =
    | Int of int64
    | String of string
    | Float of float
    | Bool of bool
    | Unit
    | List of 'expr list

type TypeDeclaration =
    | Name of string
    | Generic of string
    | Parameterized of string * TypeDeclaration list
    | Function of TypeDeclaration * TypeDeclaration

type Declaration =
    | Named of string
    | TypeAnnotated of string * TypeDeclaration