module Typechecker

// Adapted and ported from https://github.com/kseo/poly_constraints (MIT License)

open IR

type Primitive =
    | Int
    | Float
    | Bool
    | String
    | Unit

type Var = string

type Name = string

let private mergeMaps map1 map2 = Map.fold (fun acc key value -> Map.add key value acc) map1 map2

[<StructuredFormatDisplay("{AsString}")>]
type Type =
    | Primitive of Primitive
    | Func of Type * Type
    | Var of Var
    | Parameterized of string * Type list
with
    override self.ToString () =
        let parameterizedToString (name, types: Type list) = sprintf "%s %s" name (System.String.Join(" ", types))
        match self with
            | Parameterized (name, types) -> parameterizedToString (name, types)
            | Primitive str -> str.ToString ()
            | Var tp -> "'" + tp
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
    
type [<StructuredFormatDisplay("{AsString}")>] Scheme = 
    | Scheme of (Var list) * Type
with
    override self.ToString () = 
        let (Scheme (fvs, t)) = self
        match fvs with
            | [] -> t.ToString ()
            | _ -> sprintf "forall %s. %A" (System.String.Join(". ", fvs)) t

    member self.AsString = self.ToString ()

type private TempTypeData<'data> = {
    nodeData: 'data
    nodeType: Type
    mset: Set<Var>
}

let private getType (expr: IR.Expression<TempTypeData<'data>>) = (getData expr).nodeType

type DataWithType<'data> = {
    nodeData: 'data
    nodeType: Scheme
}

let getExprType (expr: IR.Expression<DataWithType<'data>>) = (getData expr).nodeType

module private Environment =
    type Env = {
        types: Map<Name, Scheme>
    }

    let keys env = env.types |> Map.toSeq |> Seq.map fst

    let fromSeq schemes = { types = Map.ofSeq schemes }

    let toSeq env = Map.toSeq env.types

module private Assumption =
    type Assumption = {
        assumptions: (Name * Type) list
    }
    
    let empty = { assumptions = [] }

    let remove assumption name = { assumptions = List.filter (fun (n, _) -> n <> name) assumption.assumptions }

    let lookup assumption name = List.filter (fun (n, _) -> n = name) assumption.assumptions |> List.map snd
    
    let merge assum1 assum2 = 
        { assumptions = List.concat [ 
            assum1.assumptions 
            assum2.assumptions ] }

    let mergeMany assumptions = { assumptions = List.concat (Seq.map (fun a -> a.assumptions) assumptions) }

    let singleton (name, tp) = { assumptions = List.singleton (name, tp) }

    let keys assumption = List.map fst assumption.assumptions

type [<StructuredFormatDisplay("{AsString}")>] private ConstraintOrigin<'data> =
    | ExprOrigin of string * 'data
    | Extern of Name
with
    override self.ToString () = match self with
                                | ExprOrigin (en, data) -> sprintf "%s: %A" en data
                                | Extern name -> sprintf "Extern: %s" name
    member self.AsString = self.ToString ()

type private Constraint<'data> =
    | EqConst of Type * Type * ConstraintOrigin<'data>
    | ExpInstConst of Type * Scheme * ConstraintOrigin<'data>
    | ImpInstConst of Type * Set<Var> * Type * ConstraintOrigin<'data>

type TypeError<'data> =
    | UnificationFail of Type * Type
    | InfiniteType of Var * Type
    | UnboundVariable of string
    | UnificationMismatch of Type list * Type list

type private FreeTypeVars<'a> = 'a -> Set<Var>

let rec private freeType: FreeTypeVars<Type> = fun t ->
    match t with
        | Primitive _ -> Set.empty
        | Var a -> Set.singleton a
        | Func (t1, t2) -> Set.union (freeType t1) (freeType t2)
        | Parameterized (n, ts) -> Set.unionMany (List.map freeType ts) 

module private Substitute =
    type Substitution = Map<Var, Type>
    
    type Substitute<'a> = Substitution -> 'a -> 'a

    let rec substType: Substitute<Type> = fun s t ->
        match t with
            | Primitive a -> Primitive a
            | Var v -> match Map.tryFind v s with
                        | None -> Var v
                        | Some t -> substType s t
            | Func (t1, t2) -> Func (substType s t1, substType s t2)
            | Parameterized (n, ts) -> Parameterized (n, List.map (substType s) ts)

    let substVarSet: Substitute<Set<Var>> = fun subst vs ->
        Set.map (fun v -> substType subst (Var v) |> freeType) vs |> Set.unionMany

let private nameGen () =
    let varNameCounter = ref 0
    
    let nextName () = 
        let x = sprintf "t%i" !varNameCounter
        incr varNameCounter
        x

    nextName

let private next n = fun () -> n () |> Var

type private Context = {
    mset: Set<Var>
    genericsMap: Map<string, Type>
}

let private emptyContext = {
    mset = Set.empty
    genericsMap = Map.empty
}

let normalize (Scheme (_, body)) = 
    let rec fv t =
        match t with
            | Var a -> [ a ]
            | Func (t1, t2) -> fv t1 @ fv t2
            | Primitive _ -> []
            | Parameterized (_, ts) -> List.collect fv ts 

    let vg = nameGen () |> next
    let ord = Seq.zip (fv body |> List.distinct) (Seq.initInfinite (fun _ -> vg ())) |> Map.ofSeq

    let rec normtype t =
        match t with
            | Primitive x -> Primitive x
            | Func (t1, t2) -> Func (normtype t1, normtype t2)
            | Var a -> Map.find a ord
            | Parameterized (n, ts) -> Parameterized (n, List.map normtype ts)
    
    let normed = normtype body
    (freeType normed |> Set.toList, normed) |> Scheme

let private occurs v t = Set.contains v (freeType t)

let rec unify t1 t2 subst =
    let bind v t =
        if t = Var v then
            subst |> Ok
        else if occurs v t then
            Error (InfiniteType (v, t))
        else
            Map.add v t subst |> Ok

    match Substitute.substType subst t1, Substitute.substType subst t2 with
        | r1, r2 when r1 = r2 -> Ok subst
        | Var v, t -> bind v t
        | t, Var v -> bind v t
        | Func (t1, t2), Func (t3, t4) ->
            Errors.result {
                let! s1 = unify t1 t3 subst
                return! unify t2 t4 s1
            }
        | Parameterized (n1, ts1), Parameterized (n2, ts2) ->
            if n1 = n2 && (List.length ts1) = (List.length ts2) then
                List.fold2 (fun s p1 p2 -> Result.bind (fun s -> unify p1 p2 s) s) (Ok subst) ts1 ts2
            else
                UnificationFail (t1, t2) |> Error
        | _ -> Error (UnificationFail (t1, t2))

let createContext (initialTypes: Map<string, AstCommon.TypeDeclaration>) (log: string -> unit) =
    let nextName = nameGen ()
    let fresh = next nextName

    let rec toType (td: AstCommon.TypeDeclaration) (context: Context) =
        match td with
            | AstCommon.Name "Int" -> Primitive Int, context
            | AstCommon.Name "String" -> Primitive String, context
            | AstCommon.Name "Float" -> Primitive Float, context
            | AstCommon.Name "Bool" -> Primitive Bool, context
            | AstCommon.Name "Unit" -> Primitive Unit, context
            | AstCommon.Name _ -> fresh (), context
            | AstCommon.Generic a -> 
                match Map.tryFind a context.genericsMap with
                    | None ->
                        let tp = fresh ()
                        tp, { context with genericsMap =  Map.add a tp context.genericsMap }
                    | Some t -> t, context
            | AstCommon.Function (itd, otd) -> 
                let it, ctx1 = toType itd context
                let ot, ctx2 = toType itd ctx1
                Func (it, ot), ctx2
            | AstCommon.Parameterized (name, parameters) ->
                let typedParams, gm = List.mapFold (fun gm p -> toType p gm) context parameters
                Parameterized (name, typedParams), gm
                
    let generalize (vars: Set<Var>) (t: Type): Scheme =
        let ts = Set.toList (Set.difference (freeType t) vars)
        (ts, t) |> Scheme

    let instantiate (Scheme (ts, t)) =
        let s = Map.ofList (List.map (fun o -> o, fresh ()) ts)
        Substitute.substType s t
    
    let rec solve subst c =
        match c with
            | EqConst (t1, t2, orig) ->
                log (sprintf "Solving: %A = %A (%A)" t1 t2 orig)
                let s1 = Substitute.substType subst t1
                let s2 = Substitute.substType subst t2
                log (sprintf "Solving (subtituted): %A = %A (%A)" s1 s2 orig)
                unify s1 s2 subst
            | ExpInstConst (t, s, orig) ->
                let s' = instantiate s
                log (sprintf "Instantiating: %A = %A (%A)" t s orig)
                solve subst (EqConst (t, s', orig))
            | ImpInstConst (t1, ms, t2, orig) ->
                log (sprintf "Generalizing: %A = %A (%A) (%A)" t1 t2 ms orig)
                let vs = Substitute.substVarSet subst ms
                let s1 = Substitute.substType subst t1
                let s2 = Substitute.substType subst t2
                log (sprintf "Generalizing (substituted): %A = %A (%A) (%A)" s1 s2 vs orig)
                solve subst (ExpInstConst (s1, generalize vs s2, orig))

    let solveAll cs = Seq.fold (fun s c -> 
        Result.bind (fun subst -> solve subst c |> Result.mapError (fun e -> e, subst)) s) (Ok Map.empty) cs

    let rec infer (expr: IR.Expression<'data>) (ctx: Context): Assumption.Assumption * Constraint<'data> seq * IR.Expression<TempTypeData<'data>> = 
        let typeData data tp = {
            nodeData = data
            nodeType = tp
            mset = ctx.mset
        }
        
        match expr with
            | IR.Variable (x, data) ->
                let tv = fresh ()
                (Assumption.singleton (x, tv), Seq.empty, IR.Variable (x, typeData data tv))
            | IR.Lambda (d, e, data) ->
                let a = nextName ()
                let tv = Var a
                let name, ta, newCtx = 
                    match d with
                        | AstCommon.Named n -> n, None, ctx
                        | AstCommon.TypeAnnotated (n, t) -> 
                            let annotatedType, ctx = toType t ctx
                            n, Some annotatedType, ctx
                let (asm, cs, subExpr) = infer e { newCtx with mset = Set.add a ctx.mset }
                let taConstraints = match ta with
                                        | None -> Seq.empty
                                        | Some annotatedType -> Seq.singleton (EqConst (tv, annotatedType, ExprOrigin ("Lambda type annotation", data)))
                let lambdaType = Func (tv, getType subExpr)
                let orig = ("Lambda", data) |> ExprOrigin
                let newCs = Seq.map (fun ts -> EqConst (ts, tv, orig)) (Assumption.lookup asm name)
                (Assumption.remove asm (AstCommon.getName d), (Seq.concat [ newCs; taConstraints; cs]), IR.Lambda (d, subExpr, typeData data lambdaType))

            | IR.Application (f, a, data) ->
                let (as1, cs1, e1) = infer f ctx
                let (as2, cs2, e2) = infer a ctx
                let t1 = getType e1
                let t2 = getType e2
                let tv = fresh ()
                let orig = ("App", data) |> ExprOrigin
                let newCs = EqConst (t1, (Func (t2, tv)), orig)
                (Assumption.merge as1 as2, Seq.append (Seq.append cs1 cs2) [ newCs ], IR.Application (e1, e2, typeData data tv))

            | IR.Let l ->
                let orig = ("Let", l.data) |> ExprOrigin
                let name, ta, newCtx = 
                    match l.binding with
                        | AstCommon.Named n -> n, None, ctx
                        | AstCommon.TypeAnnotated (n, t) -> 
                            let annotatedType, ctx = toType t ctx
                            n, Some annotatedType, ctx
                let (as1, cs1, e1) = infer l.value newCtx
                let (as2, cs2, e2) = infer l.body newCtx
                let t1 = getType e1
                let t2 = getType e2
                let taConstraints = match ta with
                                    | None -> Seq.empty
                                    | Some typeAnnotation -> Seq.singleton (EqConst (t1, typeAnnotation, orig))
                let asms = Assumption.merge as1 (Assumption.remove as2 name)
                let bindingConstraints = Seq.map (fun ts -> ImpInstConst (ts, ctx.mset, t1, orig)) (Assumption.lookup as2 name)
                let newLet = {
                    binding = l.binding
                    value = e1
                    body = e2
                    data = typeData l.data t2
                }
                (asms, Seq.concat [ cs1; taConstraints; bindingConstraints; cs2 ], IR.Let newLet)

            | IR.LetRec l ->
                let orig = ("LetRec", l.data) |> ExprOrigin
                let name, ta, newCtx = 
                    match l.binding with
                        | AstCommon.Named n -> n, None, ctx
                        | AstCommon.TypeAnnotated (n, t) -> 
                            let annotatedType, ctx = toType t ctx
                            n, Some annotatedType, ctx
                let (as1, cs1, e1) = infer l.value newCtx
                let (as2, cs2, e2) = infer l.body newCtx
                let t1 = getType e1
                let t2 = getType e2
                let taConstraints = match ta with
                                    | None -> Seq.empty
                                    | Some typeAnnotation -> Seq.singleton (EqConst (t1, typeAnnotation, orig))
                let asms = Assumption.merge (Assumption.remove as1 name) (Assumption.remove as2 name)
                let bindingConstraints = Seq.map (fun ts -> ImpInstConst (ts, ctx.mset, t1, orig)) (Assumption.lookup as2 name)
                let newLet = {
                    binding = l.binding
                    value = e1
                    body = e2
                    data = typeData l.data t2
                }
                let recursiveConstraints = Seq.map (fun ts -> EqConst (ts, t1, orig)) (Assumption.lookup as1 name)
                (asms, Seq.concat [ cs1; recursiveConstraints; taConstraints; bindingConstraints; cs2 ], IR.LetRec newLet)

            | IR.Literal (lit, data) ->
                match lit with
                    | AstCommon.Float f -> 
                        Assumption.empty, Seq.empty, (AstCommon.Float f, typeData data (Primitive Float)) |> IR.Literal
                    | AstCommon.Int i -> 
                        Assumption.empty, Seq.empty, (AstCommon.Int i, typeData data (Primitive Int)) |> IR.Literal
                    | AstCommon.Bool b -> 
                        Assumption.empty, Seq.empty, (AstCommon.Bool b, typeData data (Primitive Bool)) |> IR.Literal
                    | AstCommon.String s -> 
                        Assumption.empty, Seq.empty, (AstCommon.String s, typeData data (Primitive String)) |> IR.Literal
                    | AstCommon.Unit -> 
                        Assumption.empty, Seq.empty, (AstCommon.Unit, typeData data (Primitive Unit)) |> IR.Literal
                    | AstCommon.List exprs -> 
                        let pvar = fresh ()
                        let (assumptions, constraints, subExprs) = Seq.fold (fun (a, c, exprList) expr ->
                            let (newAs, newCs, subExpr) = infer expr ctx
                            let exprT = getType subExpr
                            let newCs = Seq.append newCs (EqConst (pvar, exprT, ExprOrigin ("List", data)) |> Seq.singleton)
                            (Assumption.merge a newAs, Seq.append c newCs, subExpr :: exprList)) (Assumption.empty, Seq.empty, []) exprs
                        let exprType =  Parameterized ("List", [ pvar ])
                        assumptions, constraints, (AstCommon.List subExprs, typeData data exprType) |> IR.Literal
            | IR.If (cond, ifExpr, elseExpr, data) ->
                let (as1, cs1, e1) = infer cond ctx
                let (as2, cs2, e2) = infer ifExpr ctx
                let (as3, cs3, e3) = infer elseExpr ctx
                let t1 = getType e1
                let t2 = getType e2
                let t3 = getType e3
                let orig = ("If", data) |> ExprOrigin
                let newCs = seq [
                    EqConst (t1, (Primitive Bool), orig)
                    EqConst (t2, t3, orig)
                ]
                (Assumption.mergeMany [ as1; as2; as3 ], Seq.concat [ cs1; cs2; cs3; newCs ], IR.If (e1, e2, e3, typeData data t2))
    
    let rec substituteAst (subst: Substitute.Substitution) (expr: IR.Expression<TempTypeData<'data>>) =
        //TODO: Normalize types
        let substData (data: TempTypeData<'data>) = { 
            nodeData = data.nodeData
            nodeType = Substitute.substType subst data.nodeType |> generalize (Substitute.substVarSet subst data.mset)
        }

        match expr with
            | IR.Variable (x, data) -> IR.Variable (x, substData data)
            | IR.Lambda (d, e, data) -> IR.Lambda (d, substituteAst subst e, substData data)
            | IR.Application (f, a, data) -> IR.Application (substituteAst subst f, substituteAst subst a, substData data)
            | IR.Let l -> 
                {
                    value = substituteAst subst l.value
                    body = substituteAst subst l.body
                    data = substData l.data
                    binding = l.binding
                } |> IR.Let
            | IR.LetRec l ->
                {
                    value = substituteAst subst l.value
                    body = substituteAst subst l.body
                    data = substData l.data
                    binding = l.binding
                } |> IR.LetRec
            | IR.Literal (lit, data) ->
                IR.Literal (AstCommon.mapLiteral (substituteAst subst) lit, substData data)
            | IR.If (condExpr, ifExpr, elseExpr, data) ->
                IR.If (substituteAst subst condExpr, substituteAst subst ifExpr, substituteAst subst elseExpr, substData data)

    let inferType env (expr: IR.Expression<'data>): Result<IR.Expression<DataWithType<'data>>, TypeError<'data>> =
        let (a, cs, e) = infer expr emptyContext
        let unbounds = Set.difference (Set.ofList (Assumption.keys a)) (Set.ofSeq (Environment.keys env))
        match Set.isEmpty unbounds with
            | false -> UnboundVariable (Set.minElement unbounds) |> Error
            | true ->
                let externConstraints = seq {
                    let e = Environment.toSeq env
                    for (x, s) in e do
                        for t in (Assumption.lookup a x) do
                            yield ExpInstConst (t, s, Extern x)
                }
                match solveAll (Seq.append externConstraints cs) with
                    | Ok subst ->
                        Map.iter (fun k v -> log (sprintf "'%s => %A" k v)) subst
                        Ok (substituteAst subst e)
                    | Error (e, subst) ->
                        Map.iter (fun k v -> log (sprintf "'%s => %A" k v)) subst
                        Error e

    let inferExpr (env: Environment.Env) (expr: IR.Expression<'data>) = 
        inferType env expr

    let initialContext = Environment.fromSeq (Seq.map (fun (n, td) -> n, fst (toType td emptyContext) |> generalize Set.empty) (Map.toSeq initialTypes))

    inferExpr initialContext