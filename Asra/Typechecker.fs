module Typechecker

open IR

type Primitive =
    | Int
    | Float
    | Bool
    | String
    | Unit

type TypeVar =
    | Unbound of string * int
    | Link of CheckerType
with
    override x.ToString () =
        match x with
            | Unbound (v, _) -> v
            | Link l -> string l

and CheckerType =
    | Primitive of Primitive
    | Func of CheckerType * CheckerType
    | Var of TypeVar ref
    | QVar of string
    | Parameterized of string * CheckerType list

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

    let newVar level = 
        let tn = sprintf "t%i" !counter
        incr counter
        Var (ref (Unbound (tn, level)))

    let rec toType (level: int) (td: AstCommon.TypeDeclaration) =
        match td with
            | AstCommon.Name "Int" -> Primitive Int
            | AstCommon.Name "String" -> Primitive String
            | AstCommon.Name "Float" -> Primitive Float
            | AstCommon.Name "Bool" -> Primitive Bool
            | AstCommon.Name "Unit" -> Primitive Unit
            | AstCommon.Name _ -> newVar level //TODO
            | AstCommon.Generic _ -> newVar level //TODO
            | AstCommon.Function (itd, otd) -> Func (toType level itd, toType level otd)
            | AstCommon.Parameterized (name, parameters) ->
                Parameterized (name, List.map (toType level) parameters)

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

    let initialContext = Map.fold (fun c k t -> addSymbol c k (toType 0 t)) Empty initialTypes

    let rec assignTypename (context: SymbolTable) (level: int) (expr: Expression<'oldData, AstCommon.Declaration>): Result<Expression<TypeData<'oldData>, Declaration>, string> =
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
                                |> List.map (assignTypename context level)
                                |> Errors.collectErrors with
                                    | exprs, [] ->
                                        let lit = AstCommon.List exprs
                                        Ok (lit, Parameterized ("List", [ newVar level ]))
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
                    let! cond = assignTypename context level condExpr
                    let! ifE = assignTypename context level ifExpr
                    let! elseE = assignTypename context level elseExpr 
                    return If (cond, ifE, elseE, {
                        nodeInformation = data
                        nodeType = newVar level
                    })
                }
            | Lambda (decl, expr, data) ->
                let (name, typ, annotated) = match decl with
                                                | AstCommon.Named n -> n, newVar level, None
                                                | AstCommon.TypeAnnotated (n, tp) -> n, newVar level, Some (toType level tp)
                let newDecl = {
                    name = name
                    declType = typ
                    annotatedType = annotated
                }
                let innerContext = addSymbol context name typ
                match assignTypename innerContext level expr with
                    | Ok newExpr ->
                        Lambda (newDecl, newExpr, {
                            nodeInformation = data
                            nodeType = newVar level
                        }) |> Ok
                    | Error e -> Error e
            | Let l ->
                let (name, typ, annotated) = match l.binding with
                                                | AstCommon.Named n -> n, newVar level, None
                                                | AstCommon.TypeAnnotated (n, tp) -> n, newVar level, Some (toType level tp)
                let newBinding = {
                    name = name
                    declType = typ
                    annotatedType = annotated
                }
                let innerContext = addSymbol context name typ
                Errors.result {
                    let! newValueExpr = assignTypename context level l.value
                    let! newBodyExpr = assignTypename innerContext level l.body
                    return Let {
                        binding = newBinding
                        value = newValueExpr
                        body = newBodyExpr
                        data = {
                            nodeInformation = l.data
                            nodeType = newVar level
                        }
                    }
                }
            | LetRec l ->
                let (name, typ, annotated) = match l.binding with
                                                | AstCommon.Named n -> n, newVar level, None
                                                | AstCommon.TypeAnnotated (n, tp) -> n, newVar level, Some (toType level tp)
                let newBinding = {
                    name = name
                    declType = typ
                    annotatedType = annotated
                }
                let innerContext = addSymbol context name typ
                Errors.result {
                    let! newValueExpr = assignTypename innerContext level l.value
                    let! newBodyExpr = assignTypename innerContext level l.body
                    return LetRec {
                        binding = newBinding
                        value = newValueExpr
                        body = newBodyExpr
                        data = {
                            nodeInformation = l.data
                            nodeType = newVar level
                        }
                    }
                }
            | Application (funcExpr, argExpr, data) ->
                Errors.result {
                    let! newFuncExpr = assignTypename context level funcExpr
                    let! newArgExpr = assignTypename context level argExpr
                    let newData = {
                        nodeInformation = data
                        nodeType = newVar level
                    }
                    return Application (newFuncExpr, newArgExpr, newData)
                }

    assignTypename initialContext 0 ir

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

let rec private occursCheck (tv: TypeVar ref) (tp: CheckerType) =
    match tp with
        | Var tv2 when tv = tv2 -> true
        | Var ({ contents = Unbound (name, level) } as tv2) ->
            let minLevel = match !tv with
                            | Unbound (_, level2) -> min level level2
                            | _ -> level
            tv2 := Unbound (name, minLevel)
            false
        | Var { contents = Link typ } -> occursCheck tv typ
        | Func (t1, t2) ->
            let o1 = occursCheck tv t1
            let o2 = occursCheck tv t2
            o1 || o2
        | _ -> false

let rec private unify (left: CheckerType) (right: CheckerType) =
    if left = right then
        Ok ()
    else
        match left, right with
            | Var ({ contents = Unbound _ } as tv), t 
            | t, Var ({ contents = Unbound _ } as tv)-> 
                match occursCheck tv t with
                    | false -> 
                        tv := Link t
                        Ok ()
                    | true -> Error (sprintf "Cannot unify types: %A contains %A" tv t)
            | Var { contents = Link t1}, t2
            | t1, Var { contents = Link t2} ->
                unify t1 t2
            | Func (ti1, to1), Func (ti2, to2) ->
                match unify ti1 ti2, unify to1 to2 with
                    | Ok _, Ok _ -> Ok ()
                    | Error e1, Error e2 -> Error (sprintf "%s; %s" e1 e2)
                    | Error a, _ 
                    | _, Error a -> Error a
            | Parameterized (ln, lps), Parameterized (rn, rps) ->
                if ln = rn && (List.length lps) = (List.length rps) then
                    match Errors.collectErrors (List.map2 (fun a b -> unify a b) lps rps) with
                        | _, [] -> Ok ()
                        | errs, _ -> System.String.Join (", ", errs) |> Error
                else
                     Error (sprintf "Cannot unify type %A with %A" left right)
            | _ -> Error (sprintf "Cannot unify type %A with %A" left right)

let unifyAll (eqs: TypeEquation<'data> seq) =
    Seq.fold (fun st eq -> 
        st 
        |> Result.bind (fun _ -> 
            unify eq.left eq.right |> Result.mapError (fun e -> sprintf "%s in %A" e (getData eq.origin).nodeInformation)))
        (Ok ()) eqs

let rec resolveType (tp: CheckerType): Types.AType =
    match tp with
        | Primitive Int -> Types.Primitive Types.Int
        | Primitive Float -> Types.Primitive Types.Float
        | Primitive Unit -> Types.Primitive Types.Unit
        | Primitive String -> Types.Primitive Types.String
        | Primitive Bool -> Types.Primitive Types.Bool
        | QVar v -> Types.QVar v
        | Var ({ contents = Unbound (x, _)}) -> Types.Var x
        | Var ({ contents = Link t}) -> resolveType t
        | Func (it, ot) ->
            Types.Func (resolveType it, resolveType ot)
        | Parameterized (name, parameters) ->
            Types.Parameterized (name, List.map resolveType parameters)