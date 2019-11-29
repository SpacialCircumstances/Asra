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

type EquationKind =
    | Eq
    | Inst
    | Gen

[<StructuredFormatDisplay("{AsString}")>]
type TypeEquation<'data> = {
    origin: Expression<TypeData<'data>, Declaration>
    left: CheckerType
    right: CheckerType
    kind: EquationKind
}
with 
    override self.ToString () =
        let q = match self.kind with
                | Eq -> ""
                | Inst -> "inst"
                | Gen -> "gen"
        sprintf "%A = %s %A" self.left q self.right
    member self.AsString = self.ToString ()

type SymbolTable = Map<string, CheckerType>

type Substitutions = Map<string, CheckerType>

type Context<'data> = {
    generateTypenames: Expression<'data, AstCommon.Declaration> ->Expression<TypeData<'data>, Declaration>
    getExprType: Expression<TypeData<'data>, Declaration> -> Substitutions -> Types.AType
    generateEquations: Expression<TypeData<'data>, Declaration> -> Result<TypeEquation<'data>, string> seq
    solveEquations: Result<TypeEquation<'data>, string> seq -> Result<Substitutions, string>
}

let createContext (initialTypes: Map<string, AstCommon.TypeDeclaration>) =
    let counter = ref 0

    let next () = 
        let tn = sprintf "t%i" !counter
        incr counter
        Var tn

    let rec toType (td: AstCommon.TypeDeclaration) =
        match td with
            | AstCommon.Name "Int" -> Primitive Int
            | AstCommon.Name "String" -> Primitive String
            | AstCommon.Name "Float" -> Primitive Float
            | AstCommon.Name "Bool" -> Primitive Bool
            | AstCommon.Name "Unit" -> Primitive Unit
            | AstCommon.Name _ -> next () //TODO
            | AstCommon.Generic _ -> next () //TODO
            | AstCommon.Function (itd, otd) -> Func (toType itd, toType otd)
            | AstCommon.Parameterized (name, parameters) ->
                Parameterized (name, List.map toType parameters)

    let rec generateTypenames (ir: Expression<'oldData, AstCommon.Declaration>): Expression<TypeData<'oldData>, Declaration> =        
        match ir with
            | Variable (name, data) ->
                Variable (name, {
                            nodeInformation = data
                            nodeType = next ()
                            })
            | Literal (lit, data) ->
                let (newLit, litType) = 
                    match lit with
                        | AstCommon.Bool b -> 
                            AstCommon.Bool b, Primitive Bool
                        | AstCommon.Int i -> 
                            AstCommon.Int i, Primitive Int
                        | AstCommon.String s -> 
                            AstCommon.String s, Primitive String
                        | AstCommon.Unit -> 
                            AstCommon.Unit, Primitive Unit
                        | AstCommon.Float f -> 
                            AstCommon.Float f, Primitive Float
                        | AstCommon.List exprs -> 
                            let newExprs = List.map generateTypenames exprs
                            AstCommon.List newExprs, Parameterized ("List", [ next () ])
                Literal (newLit, {
                    nodeInformation = data
                    nodeType = litType
                })
            | If (condExpr, ifExpr, elseExpr, data) ->
                    let cond = generateTypenames condExpr
                    let ifE = generateTypenames ifExpr
                    let elseE = generateTypenames elseExpr 
                    If (cond, ifE, elseE, {
                        nodeInformation = data
                        nodeType = next ()
                    })
            | Lambda (decl, expr, data) ->
                let (name, typ, annotated) = match decl with
                                                | AstCommon.Named n -> n, next (), None
                                                | AstCommon.TypeAnnotated (n, tp) -> n, next (), Some (toType tp)
                let newDecl = {
                    name = name
                    declType = typ
                    annotatedType = annotated
                }
                Lambda (newDecl, generateTypenames expr, {
                    nodeInformation = data
                    nodeType = next ()
                })
            | Let l ->
                let (name, typ, annotated) = match l.binding with
                                                | AstCommon.Named n -> n, next (), None
                                                | AstCommon.TypeAnnotated (n, tp) -> n, next (), Some (toType tp)
                let newBinding = {
                    name = name
                    declType = typ
                    annotatedType = annotated
                }
                let newValueExpr = generateTypenames l.value
                let newBodyExpr = generateTypenames l.body
                Let {
                    binding = newBinding
                    value = newValueExpr
                    body = newBodyExpr
                    data = {
                        nodeInformation = l.data
                        nodeType = next ()
                    }
                }
            | LetRec l ->
                let (name, typ, annotated) = match l.binding with
                                                | AstCommon.Named n -> n, next (), None
                                                | AstCommon.TypeAnnotated (n, tp) -> n, next (), Some (toType tp)
                let newBinding = {
                    name = name
                    declType = typ
                    annotatedType = annotated
                }
                let newValueExpr = generateTypenames l.value
                let newBodyExpr = generateTypenames l.body
                LetRec {
                    binding = newBinding
                    value = newValueExpr
                    body = newBodyExpr
                    data = {
                        nodeInformation = l.data
                        nodeType = next ()
                    }
                }
            | Application (funcExpr, argExpr, data) ->
                let newFuncExpr = generateTypenames funcExpr
                let newArgExpr = generateTypenames argExpr
                let newData = {
                    nodeInformation = data
                    nodeType = next ()
                }
                Application (newFuncExpr, newArgExpr, newData)
    
    let rec resolveSymbol (context: SymbolTable) (name: string) = Map.tryFind name context
    
    let addSymbol (context: SymbolTable) (name: string) (typ: CheckerType) = Map.add name typ context
    
    let initialContext = Map.fold (fun c k t -> addSymbol c k (toType t)) Map.empty initialTypes

    let getType (expr: Expression<TypeData<'data>, 'decl>) = (getData expr).nodeType
    
    let generateEquations (expr: Expression<TypeData<'data>, Declaration>) =
        let eq l r = Ok {
            left = l
            right = r
            origin = expr
            kind = Eq
        }

        let eqInst l r = Ok {
            left = l
            right = r
            origin = expr
            kind = Inst
        }

        let eqGen l r = Ok {
            left = l
            right = r
            origin = expr
            kind = Gen
        }

        let rec genEq (context: SymbolTable) (expr: Expression<TypeData<'data>, Declaration>) =
            seq {
                match expr with
                    | Application (funcExpr, argExpr, data) ->
                        yield! genEq context funcExpr
                        yield! genEq context argExpr
                        yield eq (getType funcExpr) (Func (getType argExpr, data.nodeType))
                    | If (condExpr, ifExpr, elseExpr, data) ->
                        yield! genEq context condExpr
                        yield! genEq context ifExpr
                        yield! genEq context elseExpr
                        yield eq (getType ifExpr) (Primitive Bool)
                        yield eq data.nodeType (getType ifExpr)
                        yield eq data.nodeType (getType elseExpr)
                    | Variable (name, data) ->
                        match resolveSymbol context name with
                                        | Some varType -> 
                                            yield eqInst data.nodeType varType
                                        | None -> 
                                            yield (Error (sprintf "Value %s not defined in %A" name data.nodeInformation))
                    | Lambda (decl, expr, data) ->
                        let newContext = addSymbol context decl.name decl.declType
                        yield! genEq newContext expr
                        match decl.annotatedType with
                                | None -> ()
                                | Some annotated ->
                                    yield eq decl.declType annotated
                        yield eq data.nodeType (Func (decl.declType, getType expr))
                    | Let l ->
                        yield! genEq context l.value
                        match l.binding.annotatedType with
                                | None -> ()
                                | Some annotated ->
                                    yield eq l.binding.declType annotated
                        yield eqGen l.binding.declType (getType l.value)
                        let newContext = addSymbol context l.binding.name l.binding.declType
                        yield! genEq newContext l.body
                        yield eq l.data.nodeType (getType l.body)
                    | LetRec l ->
                        let innerContext = addSymbol context l.binding.name l.binding.declType
                        yield! genEq innerContext l.value
                        match l.binding.annotatedType with
                                | None -> ()
                                | Some annotated ->
                                    yield eq l.binding.declType annotated
                        yield eqGen l.binding.declType (getType l.value)
                        let newContext = addSymbol context l.binding.name l.binding.declType
                        yield! genEq newContext l.body
                        yield eq l.data.nodeType (getType l.body)
                    | Literal (lit, data) ->
                        match lit with
                            | AstCommon.Literal.List exprs ->
                                match data.nodeType with
                                    | Parameterized ("List", [elType]) ->
                                        for expr in exprs do
                                            yield! genEq context expr
                                            yield eq elType (getType expr)
                                    | _ -> invalidOp "Expected parameterized list type for list literal"
                                ()
                            | _ -> ()
            }

        genEq initialContext expr
    
    let rec occursCheck (subst: Substitutions) (a: CheckerType) (b: CheckerType) =
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
    
    let rec unifyVariable (subst: Substitutions) (a: CheckerType) (b: CheckerType) unify =
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
    
    let rec unify (subst: Substitutions) (left: CheckerType) (right: CheckerType) =
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
    
    let rec inst (subst: Substitutions) (tp: CheckerType) = 
        let rec replace subst t =
            match t with
                | Var n -> 
                    match Map.tryFind n subst with
                        | None -> Var n
                        | Some x -> replace subst x
                | Func (f1, f2) -> Func (replace subst f1, replace subst f2)
                | Parameterized (name, tps) -> Parameterized (name, List.map (replace subst) tps)
                | _ -> t

        match tp with
            | Scheme (free, tp) ->
                let subst = free
                            |> Seq.map (fun x -> x, next ())
                            |> Map.ofSeq
                replace subst tp
            | Func (f1, f2) -> Func (inst subst f1, inst subst f2)
            | Parameterized (n, tps) -> Parameterized (n, List.map (inst subst) tps)
            | Var n ->
                match Map.tryFind n subst with
                    | None -> Var n
                    | Some t -> inst subst t
            | _ -> tp

    let rec gen (subst: Substitutions) (tp: CheckerType) = 
        //TODO: Make this sound
        let rec collect t free =
            match t with
            | Var n ->
                match Map.tryFind n subst with
                    | None -> Set.add n free
                    | Some x -> Set.union (collect x free) free
            | Func (f1, f2) -> Set.union (collect f1 free) (collect f2 free)
            | Parameterized (_, tps) -> Set.unionMany (List.map (fun t -> collect t free) tps)
            | _ -> free

        let frees = collect tp Set.empty
        Scheme (frees, tp)

    let quantifyEquation (eq: TypeEquation<'data>) (subst: Substitutions) =
        match eq.kind with
            | Eq -> eq.left, eq.right
            | Inst -> eq.left, inst subst eq.right
            | Gen -> eq.left, gen subst eq.right

    let unifyAll (eqs: Result<TypeEquation<'data>, string> seq) =
        Seq.fold (fun st eq -> 
            match st, eq with
                | Ok subst, Ok eq -> 
                    let (left, right) = quantifyEquation eq subst
                    unify subst left right |> Result.mapError (fun e -> sprintf "%s in %A" e (getData eq.origin).nodeInformation)
                | Error e1, Error e2 -> Error (e1 + e2)
                | Error e, _
                | _, Error e -> Error e)
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

    {
        generateEquations = generateEquations
        generateTypenames = generateTypenames
        solveEquations = unifyAll
        getExprType = fun expr subst -> getType expr |> resolveType subst
    }