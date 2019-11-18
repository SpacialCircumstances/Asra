module Prelude

open AstCommon

let context: Map<string, TypeDeclaration> = Map.ofList [
    "println", Function (Generic "a", Name "Unit")
]