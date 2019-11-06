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
    | Parameterized of string * AType list

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

    let rec assignTypename (context: SymbolTable) (expr: Expression<'oldData>): Result<Expression<TypeData<'oldData>>, string> =
        match expr with
            | Variable (name, data) when Map.containsKey name context ->
                Variable (name, {
                    nodeInformation = data
                    nodeType = Map.find name context
                    }) |> Ok
            | Literal (lit, data) ->
                let newLit = 
                    match lit with
                        | AstCommon.Bool b -> 
                            Ok (AstCommon.Bool b, Primitive Bool)
                        | AstCommon.Int i -> 
                            Ok (AstCommon.Int i, Primitive Int)
                        | AstCommon.String s -> 
                            Ok (AstCommon.String s, Primitive String)
                        | AstCommon.Unit -> 
                            Ok (AstCommon.Unit, Primitive Unit)
                        | AstCommon.Float f -> 
                            Ok (AstCommon.Float f, Primitive Float)
                        | AstCommon.List exprs -> 
                            match exprs 
                                |> List.map (assignTypename context)
                                |> Errors.collectErrors with
                                    | exprs, [] ->
                                        let lit = AstCommon.List exprs
                                        Ok (lit, Parameterized ("List", [ Var (next ()) ]))
                                    | _, errs -> Error errs
                match newLit with
                    | Ok (literal, litType) ->
                        Literal (literal, {
                            nodeInformation = data
                            nodeType = litType
                        }) |> Ok
                    | Error errors -> Error (System.String.Join (", ", errors))
            | If (condExpr, ifExpr, elseExpr, data) ->
                Errors.result {
                    let! cond = assignTypename context condExpr
                    let! ifE = assignTypename context ifExpr
                    let! elseE = assignTypename context elseExpr 
                    return If (cond, ifE, elseE, {
                        nodeInformation = data
                        nodeType = Var (next ())
                    })
                }
            | _ -> Error ""

    assignTypename Map.empty ir