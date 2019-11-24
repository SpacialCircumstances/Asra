module Typechecker

open IR

type Primitive =
    | Int
    | Float
    | Bool
    | String
    | Unit

[<StructuredFormatDisplay("{AsString}")>]
type CheckerType =
    | Primitive of Primitive
    | Func of CheckerType * CheckerType
    | Var of string
    | Scheme of Set<string> * CheckerType
    | Parameterized of string * CheckerType list
with
    override self.ToString () =
        let parameterizedToString (name, types: CheckerType list) = sprintf "%s %s" name (System.String.Join(" ", types))
        match self with
            | Parameterized (name, types) -> parameterizedToString (name, types)
            | Primitive str -> str.ToString ()
            | Var tp -> "'" + tp
            | Scheme (gs, tp) -> sprintf "forall %s. %A" (System.String.Join(". ", gs)) tp
            | Func (input, output) -> 
                match input with
                    | Var tp ->
                        sprintf "'%s -> %O" tp output
                    | Parameterized (name, types) ->
                        sprintf "(%s) -> %O" (parameterizedToString (name, types)) output
                    | Func (fti, fto) ->
                        sprintf "(%O -> %O) -> %O" fti fto output
                    | _ -> sprintf "%O -> %O" input output
    member self.AsString = self.ToString()

[<StructuredFormatDisplay("{AsString}")>]
type TypeData<'oldData> = {
    nodeInformation: 'oldData
    nodeType: CheckerType
}
with
    override self.ToString () = sprintf "{ type = %A @%A }" self.nodeType self.nodeInformation
    member self.AsString = self.ToString ()

[<StructuredFormatDisplay("{AsString}")>]
type Declaration = {
    name: string
    declType: CheckerType
    annotatedType: CheckerType option
}
with
    override self.ToString () = sprintf "(%s: %A)" self.name self.declType
    member self.AsString = self.ToString ()

[<StructuredFormatDisplay("{left} = {right}")>]
type TypeEquation<'data> = {
    origin: Expression<TypeData<'data>, Declaration>
    left: CheckerType
    right: CheckerType
}

type SymbolTable = 
    | Empty
    | Data of string * CheckerType * SymbolTable

type Substitutions = Map<string, CheckerType>

let generateTypenames (initialTypes: Map<string, AstCommon.TypeDeclaration>) (ir: Expression<'oldData, AstCommon.Declaration>): Result<Expression<TypeData<'oldData>, Declaration>, string> =
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

    let rec containsSymbol (context: SymbolTable) (name: string) = 
        match context with
            | Empty -> false
            | Data (n, _, _) when n = name -> true
            | Data (_, _, p) -> containsSymbol p name

    let rec resolveSymbol (context: SymbolTable) (name: string) = 
        match context with
            | Empty -> invalidOp (sprintf "Error resolving symbol %s" name)
            | Data (n, tp, _) when n = name -> tp
            | Data (_, _, p) -> resolveSymbol p name

    let addSymbol (context: SymbolTable) (name: string) (typ: CheckerType) = Data (name, typ, context)

    let initialContext = Map.fold (fun c k t -> addSymbol c k (toType t)) Empty initialTypes

    let rec assignTypename (context: SymbolTable) (expr: Expression<'oldData, AstCommon.Declaration>): Result<Expression<TypeData<'oldData>, Declaration>, string> =
        match expr with
            | Variable (name, data) when containsSymbol context name ->
                Variable (name, {
                    nodeInformation = data
                    nodeType = resolveSymbol context name
                    }) |> Ok
            | Variable (name, data) ->
                Error (sprintf "Error: Variable %s not defined in %O" name data)
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
                let (name, typ, annotated) = match decl with
                                                | AstCommon.Named n -> n, Var (next ()), None
                                                | AstCommon.TypeAnnotated (n, tp) -> n, Var (next ()), Some (toType tp)
                let newDecl = {
                    name = name
                    declType = typ
                    annotatedType = annotated
                }
                let innerContext = addSymbol context name typ
                match assignTypename innerContext expr with
                    | Ok newExpr ->
                        Lambda (newDecl, newExpr, {
                            nodeInformation = data
                            nodeType = Var (next ())
                        }) |> Ok
                    | Error e -> Error e
            | Let l ->
                let (name, typ, annotated) = match l.binding with
                                                | AstCommon.Named n -> n, Var (next ()), None
                                                | AstCommon.TypeAnnotated (n, tp) -> n, Var (next ()), Some (toType tp)
                let newBinding = {
                    name = name
                    declType = typ
                    annotatedType = annotated
                }
                let innerContext = addSymbol context name typ
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
                let (name, typ, annotated) = match l.binding with
                                                | AstCommon.Named n -> n, Var (next ()), None
                                                | AstCommon.TypeAnnotated (n, tp) -> n, Var (next ()), Some (toType tp)
                let newBinding = {
                    name = name
                    declType = typ
                    annotatedType = annotated
                }
                let innerContext = addSymbol context name typ
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
                    return Application (newFuncExpr, newArgExpr, newData)
                }

    assignTypename initialContext ir

let getType (expr: Expression<TypeData<'data>, 'decl>) = (getData expr).nodeType

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
                match decl.annotatedType with
                        | None -> ()
                        | Some annotated ->
                            yield eq decl.declType annotated
                yield eq data.nodeType (Func (decl.declType, getType expr))
            | Let l ->
                yield! generateEquations l.value
                match l.binding.annotatedType with
                        | None -> ()
                        | Some annotated ->
                            yield eq l.binding.declType annotated
                yield eq l.binding.declType (getType l.value)
                yield! generateEquations l.body
                yield eq l.data.nodeType (getType l.body)
            | LetRec l ->
                yield! generateEquations l.value
                match l.binding.annotatedType with
                        | None -> ()
                        | Some annotated ->
                            yield eq l.binding.declType annotated
                yield eq l.binding.declType (getType l.value)
                yield! generateEquations l.body
                yield eq l.data.nodeType (getType l.body)
            | Literal (lit, data) ->
                match lit with
                    | AstCommon.Literal.List exprs ->
                        match data.nodeType with
                            | Parameterized ("List", [elType]) ->
                                for expr in exprs do
                                    yield! generateEquations expr
                                    yield eq elType (getType expr)
                            | _ -> invalidOp "Expected parameterized list type for list literal"
                        ()
                    | _ -> ()
    }

let rec private occursCheck (subst: Substitutions) (a: CheckerType) (b: CheckerType) =
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

let rec private unifyVariable (subst: Substitutions) (a: CheckerType) (b: CheckerType) unify =
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

let rec private unify (subst: Substitutions) (left: CheckerType) (right: CheckerType) =
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
    Seq.fold (fun st eq -> 
        st 
        |> Result.bind (fun subst -> 
            unify subst eq.left eq.right |> Result.mapError (fun e -> sprintf "%s in %A" e (getData eq.origin).nodeInformation)))
        (Ok Map.empty) eqs

let rec resolveType (subst: Substitutions) (tp: CheckerType): Types.AType =
    match tp with
        | Primitive Int -> Types.Primitive Types.Int
        | Primitive Float -> Types.Primitive Types.Float
        | Primitive Unit -> Types.Primitive Types.Unit
        | Primitive String -> Types.Primitive Types.String
        | Primitive Bool -> Types.Primitive Types.Bool
        | Scheme (gs, t) -> Types.Scheme (gs, resolveType subst t)
        | Var s ->
            match Map.tryFind s subst with
                | Some t -> resolveType subst t
                | None -> Types.Var s
        | Func (it, ot) ->
            Types.Func (resolveType subst it, resolveType subst ot)
        | Parameterized (name, parameters) ->
            Types.Parameterized (name, List.map (resolveType subst) parameters)