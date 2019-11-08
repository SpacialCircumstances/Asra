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

type TypeEquation<'data> = {
    origin: Expression<TypeData<'data>>
    left: AType
    right: AType
}

type SymbolTable = Map<string, AType>

let generateTypenames (ir: Expression<'oldData>): Result<Expression<TypeData<'oldData>>, string> =
    let counter = ref 0
    let next () = 
        let tn = sprintf "t%i" !counter
        incr counter
        tn

    let rec toType (td: AstCommon.TypeDeclaration) =
        match td with
            | AstCommon.Name "Int" -> Primitive Int
            | AstCommon.Name "String" -> Primitive String
            | AstCommon.Name "Float" -> Primitive Float
            | AstCommon.Name "Bool" -> Primitive Bool
            | AstCommon.Name "Unit" -> Primitive Unit
            | AstCommon.Name _ -> Var (next ()) //TODO
            | AstCommon.Generic _ -> Var (next ()) //TODO
            | AstCommon.Function (itd, otd) -> Func (toType itd, toType otd)
            | AstCommon.Parameterized (name, parameters) ->
                Parameterized (name, List.map toType parameters)

    let rec assignTypename (context: SymbolTable) (expr: Expression<'oldData>): Result<Expression<TypeData<'oldData>>, string> =
        match expr with
            | Variable (name, data) when Map.containsKey name context ->
                Variable (name, {
                    nodeInformation = data
                    nodeType = Map.find name context
                    }) |> Ok
            | Variable (name, data) ->
                Error (sprintf "Error in %O: Variable %s not defined" data name)
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
            | Lambda (decl, expr, data) ->
                let (name, typ) = match decl with
                                    | AstCommon.Named n -> n, Var (next ())
                                    | AstCommon.TypeAnnotated (n, tp) -> n, toType tp
                let innerContext = Map.add name typ context
                match assignTypename innerContext expr with
                    | Ok newExpr ->
                        Lambda (decl, newExpr, {
                            nodeInformation = data
                            nodeType = typ
                        }) |> Ok
                    | Error e -> Error e
            | Let l ->
                let (name, typ) = match l.binding with
                                    | AstCommon.Named n -> n, Var (next ())
                                    | AstCommon.TypeAnnotated (n, tp) -> n, toType tp
                let innerContext = Map.add name typ context
                Errors.result {
                    let! newValueExpr = assignTypename context l.value
                    let! newBodyExpr = assignTypename innerContext l.body
                    return Let {
                        binding = l.binding
                        value = newValueExpr
                        body = newBodyExpr
                        data = {
                            nodeInformation = l.data
                            nodeType = Var (next ())
                        }
                    }
                }
            | LetRec l ->
                let (name, typ) = match l.binding with
                                    | AstCommon.Named n -> n, Var (next ())
                                    | AstCommon.TypeAnnotated (n, tp) -> n, toType tp
                let innerContext = Map.add name typ context
                Errors.result {
                    let! newValueExpr = assignTypename context l.value
                    let! newBodyExpr = assignTypename innerContext l.body
                    return LetRec {
                        binding = l.binding
                        value = newValueExpr
                        body = newBodyExpr
                        data = {
                            nodeInformation = l.data
                            nodeType = Var (next ())
                        }
                    }
                }
            | Application (funcExpr, argExpr, data) ->
                Errors.result {
                    let! newFuncExpr = assignTypename context funcExpr
                    let! newArgExpr = assignTypename context argExpr
                    let newData = {
                        nodeInformation = data
                        nodeType = Var (next ())
                    }
                    return Application(newFuncExpr, newArgExpr, newData)
                }

    assignTypename Map.empty ir

let getType (expr: Expression<TypeData<'data>>) = (getData expr).nodeType

let rec generateEquations (expr: Expression<TypeData<'data>>) =
    seq {
        match expr with
            | Application (funcExpr, argExpr, data) ->
                yield! generateEquations funcExpr
                yield! generateEquations argExpr
                yield {
                    left = getType funcExpr
                    right = Func (getType argExpr, data.nodeType)
                    origin = expr
                }
    }