module Typechecker

open IR

type Primitive =
    | Int
    | Float
    | Bool
    | String
    | Unit

type AType =
    | Primitive of Primitive
    | Func of AType * AType
    | Var of string

type TypeData<'oldData> = {
    nodeInformation: 'oldData
    nodeType: AType
}

type SymbolTable = Map<string, AType>

let generateTypenames (ir: Expression<'oldData>): Result<Expression<TypeData<'oldData>>, string> =
    let counter = ref 0
    let next () = 
        let tn = sprintf "t%i" !counter
        incr counter
        tn

    let rec assignTypename (expr: Expression<'oldData>) (context: SymbolTable): Result<Expression<TypeData<'oldData>>, string> =
        match expr with
            | Variable (name, data) when Map.containsKey name context ->
                Variable (name, {
                    nodeInformation = data
                    nodeType = Map.find name context
                    }) |> Ok
            | _ -> Error ""

    assignTypename ir Map.empty