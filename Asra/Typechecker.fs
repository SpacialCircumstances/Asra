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

type Declaration = {
    name: string
    declType: AType
}

type TypeEquation<'data> = {
    origin: Expression<TypeData<'data>, Declaration>
    left: AType
    right: AType
}

type SymbolTable = Map<string, AType>

type Substitutions = Map<string, AType>

let generateTypenames (ir: Expression<'oldData, AstCommon.Declaration>): Result<Expression<TypeData<'oldData>, Declaration>, string> =
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

    let rec assignTypename (context: SymbolTable) (expr: Expression<'oldData, AstCommon.Declaration>): Result<Expression<TypeData<'oldData>, Declaration>, string> =
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
                let newDecl = {
                    name = name
                    declType = typ
                }
                let innerContext = Map.add name typ context
                match assignTypename innerContext expr with
                    | Ok newExpr ->
                        Lambda (newDecl, newExpr, {
                            nodeInformation = data
                            nodeType = typ
                        }) |> Ok
                    | Error e -> Error e
            | Let l ->
                let (name, typ) = match l.binding with
                                    | AstCommon.Named n -> n, Var (next ())
                                    | AstCommon.TypeAnnotated (n, tp) -> n, toType tp
                let newBinding = {
                    name = name
                    declType = typ
                }
                let innerContext = Map.add name typ context
                Errors.result {
                    let! newValueExpr = assignTypename context l.value
                    let! newBodyExpr = assignTypename innerContext l.body
                    return Let {
                        binding = newBinding
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
                let newBinding = {
                    name = name
                    declType = typ
                }
                let innerContext = Map.add name typ context
                Errors.result {
                    let! newValueExpr = assignTypename innerContext l.value
                    let! newBodyExpr = assignTypename innerContext l.body
                    return LetRec {
                        binding = newBinding
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

let private getType (expr: Expression<TypeData<'data>, 'decl>) = (getData expr).nodeType

let rec generateEquations (expr: Expression<TypeData<'data>, Declaration>) =
    let eq l r = {
        left = l
        right = r
        origin = expr
    }
    seq {
        match expr with
            | Application (funcExpr, argExpr, data) ->
                yield! generateEquations funcExpr
                yield! generateEquations argExpr
                yield eq (getType funcExpr) (Func (getType argExpr, data.nodeType))
            | If (condExpr, ifExpr, elseExpr, data) ->
                yield! generateEquations condExpr
                yield! generateEquations ifExpr
                yield! generateEquations elseExpr
                yield eq (getType ifExpr) (Primitive Bool)
                yield eq data.nodeType (getType ifExpr)
                yield eq data.nodeType (getType elseExpr)
            | Variable _ -> ()
            | Lambda (decl, expr, data) ->
                yield! generateEquations expr
                yield eq data.nodeType (Func (decl.declType, getType expr))
            | Let l ->
                yield! generateEquations l.value
                yield eq l.binding.declType (getType l.value)
                yield! generateEquations l.body
                yield eq l.data.nodeType (getType l.body)
            | LetRec l ->
                yield! generateEquations l.value
                yield eq l.binding.declType (getType l.value)
                yield! generateEquations l.body
                yield eq l.data.nodeType (getType l.body)
            | Literal (lit, data) ->
                //In theory, we only need to infer list literals here, and we will do this later
                ()
    }

let rec private occursCheck (subst: Substitutions) (a: AType) (b: AType) =
    if a = b then
        true
    else
        match b with
            | Var bVar when Map.containsKey bVar subst ->
                occursCheck subst a (Map.find bVar subst) 
            | Func (bi, bo) ->
                occursCheck subst a bi || occursCheck subst a bo
            | Parameterized (_, tps) ->
                List.exists (fun tp -> occursCheck subst a tp) tps
            | _ -> false

let rec private unifyVariable (subst: Substitutions) (a: AType) (b: AType) unify =
    match a with
        | Var aVar ->
            match Map.tryFind aVar subst with
                | Some tp -> unify subst tp b
                | None ->
                    match b with
                        | Var bvar when Map.containsKey bvar subst ->
                            unifyVariable subst a (Map.find bvar subst) unify
                        | _ when occursCheck subst a b ->
                            Error (sprintf "Cannot unify type %A with %A" a b)
                        | _ -> 
                            Map.add aVar b subst |> Ok
        | _ -> invalidOp "Expected Var type"

let rec private unify (subst: Substitutions) (left: AType) (right: AType) =
    if left = right then
        Ok subst
    else
        match left, right with
            | Var _, _ -> unifyVariable subst left right unify
            | _, Var _ -> unifyVariable subst right left unify
            | Func (li, lo), Func (ri, ro) ->
                Result.bind (fun subst -> unify subst lo ro) (unify subst li ri)
            | Parameterized (ln, lps), Parameterized (rn, rps) ->
                if ln = rn && (List.length lps) = (List.length rps) then
                    List.fold2 (fun s a b -> Result.bind (fun s -> unify s a b) s) (Ok subst) lps rps
                else
                     Error (sprintf "Cannot unify type %A with %A" left right)
            | _ -> Error (sprintf "Cannot unify type %A with %A" left right)

let unifyAll (eqs: TypeEquation<'data> seq) =
    Seq.fold (fun st eq -> Result.bind (fun subst -> unify subst eq.left eq.right) st) (Ok Map.empty) eqs