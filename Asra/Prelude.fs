module Prelude

open AstCommon

let private tunit = Name "Unit"
let private tbool = Name "Bool"
let private tint = Name "Int"
let private tfloat = Name "Float"
let private tstring = Name "String"

let context: Map<string, TypeDeclaration> = Map.ofList [
    "println", Function (Generic "a", tunit)
    "==", Function (Generic "a", Function (Generic "a", tbool))
    "-", Function (tint, Function (tint, tint))
    "+", Function (tint, Function (tint, tint))
    "*", Function (tint, Function (tint, tint))
    "/", Function (tint, Function (tint, tint))
    ".+", Function (tfloat, Function (tfloat, tfloat))
    ".-", Function (tfloat, Function (tfloat, tfloat))
    ".*", Function (tfloat, Function (tfloat, tfloat))
    "./", Function (tfloat, Function (tfloat, tfloat))
    "intToFloat", Function (tint, tfloat)
    "floatToInt", Function (tfloat, tint)
    "toString", Function (Generic "a", tstring)
]