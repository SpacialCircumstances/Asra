module Typechecker

open IR

type Primitive =
    | Int
    | Float
    | Bool
    | String
    | Unit

type Var = string

type Name = string

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


module Environment =
    let mergeMaps map1 map2 = Map.fold (fun acc key value -> Map.add key value acc) map1 map2

    type Env = {
        types: Map<Name, Scheme>
    }

    let empty = { types = Map.empty }

    let extend env (name, scheme) = { types = Map.add name scheme env.types }

    let remove env name = { types = Map.remove name env.types }

    let extends env bindings = { types = mergeMaps (Map.ofList bindings) env.types }

    let lookup env name = Map.tryFind name env

    let merge env1 env2 = { types = mergeMaps env1.types env2.types }

    let mergeMany envs = { types = Seq.fold mergeMaps Map.empty envs }

    let singleton name scheme = { types = Map.ofList [ name, scheme ] }

    let keys env = env.types |> Map.toSeq |> Seq.map fst

    let fromSeq schemes = { types = Map.ofSeq schemes }

    let toSeq env = Map.toSeq env.types

module Assumption =
    type Assumption = {
        assumptions: (Name * Type) list
    }
    
    let empty = { assumptions = [] }

    let extend assumption (name, tp) = { assumptions = (name, tp) :: assumption.assumptions }

    let remove assumption name = { assumptions = List.filter (fun (n, _) -> n <> name) assumption.assumptions }

    let lookup assumption name = List.filter (fun (n, _) -> n = name) assumption.assumptions |> List.map snd
    
    let merge assum1 assum2 = 
        { assumptions = List.concat [ 
            assum1.assumptions 
            assum2.assumptions ] }

    let mergeMany assumptions = { assumptions = List.concat (Seq.map (fun a -> a.assumptions) assumptions) }

    let singleton (name, tp) = { assumptions = List.singleton (name, tp) }

    let keys assumption = List.map fst assumption.assumptions

type [<StructuredFormatDisplay("{AsString}")>] Constraint =
    | EqConst of Type * Type
    | ExpInstConst of Type * Scheme
    | ImpInstConst of Type * Set<Var> * Type
with
    override self.ToString () = match self with
                                    | EqConst (t1, t2) -> sprintf "%A = %A" t1 t2
                                    | ImpInstConst (t1, m, t2) -> sprintf "%A = %A (%A)" t1 t2 m
                                    | ExpInstConst (t1, s) -> sprintf "%A = %A" t1 s
    member self.AsString = self.ToString ()

type TypeError =
    | UnificationFail of Type * Type
    | InfiniteType of Var * Type
    | UnboundVariable of string
    | Ambigious of Constraint list
    | UnificationMismatch of Type list * Type list

module Substitute =
    type Substitution = Map<Var, Type>
    
    type Substitute<'a> = Substitution -> 'a -> 'a

    let substMany = fun s (l: 'a seq) (f: Substitute<'a>) -> Seq.map (f s) l

    let rec substType: Substitute<Type> = fun s t ->
        match t with
            | Primitive a -> Primitive a
            | Var v -> match Map.tryFind v s with
                        | None -> Var v
                        | Some t -> substType s t
            | Func (t1, t2) -> Func (substType s t1, substType s t2)
            | Parameterized _ -> invalidOp "Not implemented"

    let substScheme: Substitute<Scheme> = fun s (Scheme (ts, t)) ->
        (ts, substType (List.foldBack Map.remove ts s) t) |> Scheme

module TypeVars =
    type FreeTypeVars<'a> = 'a -> Set<Var>

    type ActiveTypeVars<'a> = 'a -> Set<Var>

    let freeMany (l: 'a seq) (f: FreeTypeVars<'a>) = Set.unionMany (Seq.map f l)

    let freeSet (s: Set<'a>) (f: FreeTypeVars<'a>) = Set.unionMany (Seq.map f (Set.toSeq s))

    let rec freeType: FreeTypeVars<Type> = fun t ->
        match t with
            | Primitive _ -> Set.empty
            | Var a -> Set.singleton a
            | Func (t1, t2) -> Set.union (freeType t1) (freeType t2)
            | Parameterized _ -> invalidOp "Not implemented"

    let freeScheme: FreeTypeVars<Scheme> = fun (Scheme (ts, t)) -> Set.difference (freeType t) (Set.ofList ts)

    let activeConstraint: ActiveTypeVars<Constraint> = fun c ->
        match c with
            | EqConst (t1, t2) -> Set.union (freeType t1) (freeType t2)
            | ExpInstConst (t, s) -> Set.union (freeType t) (freeScheme s)
            | ImpInstConst (t1, m, t2) -> Set.union (freeType t1) (Set.intersect m (freeType t2))

    let activeMany (l: 'a seq) (f: ActiveTypeVars<'a>) = Set.unionMany (Seq.map f l)

let nameGen () =
    let varNameCounter = ref 0
    
    let nextName () = 
        let x = sprintf "t%i" !varNameCounter
        incr varNameCounter
        x

    nextName

let next n = fun () -> n () |> Var

let createContext (initialTypes: Map<string, AstCommon.TypeDeclaration>) =
    let nextName = nameGen ()
    let fresh = next nextName

    let generalize (vars: Set<Var>) (t: Type): Scheme =
        let ts = Set.toList (Set.difference (TypeVars.freeType t) vars)
        (ts, t) |> Scheme

    let instantiate (Scheme (ts, t)) =
        let s = Map.ofList (List.map (fun o -> o, fresh ()) ts)
        Substitute.substType s t

    let normalize (Scheme (_, body)) = 
        let rec fv t =
            match t with
                | Var a -> [ a ]
                | Func (t1, t2) -> fv t1 @ fv t2
                | Primitive _ -> []
                | Parameterized _ -> invalidOp "Not implemented"

        let vg = nameGen () |> next
        let ord = Seq.zip (fv body |> List.distinct) (Seq.initInfinite (fun _ -> vg ())) |> Map.ofSeq

        let rec normtype t =
            match t with
                | Primitive x -> Primitive x
                | Func (t1, t2) -> Func (normtype t1, normtype t2)
                | Var a -> Map.find a ord
                | Parameterized _ -> invalidOp "Not implemented"
        
        let normed = normtype body
        (TypeVars.freeType normed |> Set.toList, normed) |> Scheme

    let closeOver (t: Type): Scheme = generalize Set.empty t |> normalize
    
    let occurs v t = Set.contains v (TypeVars.freeType t)

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
                invalidOp "Not implemented"
            | _ -> Error (UnificationFail (t1, t2))

    let rec solve subst c =
        match c with
            | EqConst (t1, t2) ->
                unify t1 t2 subst
            | ExpInstConst (t, s) ->
                let s' = instantiate s
                solve subst (EqConst (t, s'))
            | ImpInstConst (t1, ms, t2) ->
                solve subst (ExpInstConst (t1, generalize ms t2))

    let solveAll cs = Seq.fold (fun s c -> 
        printfn "Solving: %A" c
        Result.bind (fun subst -> solve subst c) s) (Ok Map.empty) cs

    let rec infer (expr: IR.Expression<'data, AstCommon.Declaration>) (mset: Set<Var>): Assumption.Assumption * Constraint seq * Type = 
        match expr with
            | IR.Variable (x, data) ->
                let tv = fresh ()
                (Assumption.singleton (x, tv), Seq.empty, tv)
            | IR.Lambda (d, e, data) ->
                let a = nextName ()
                let tv = Var a
                let (asm, cs, t) = infer e (Set.add a mset)
                let name = AstCommon.getName d
                let newCs = Seq.map (fun ts -> EqConst (ts, tv)) (Assumption.lookup asm name)
                (Assumption.remove asm (AstCommon.getName d), (Seq.append cs newCs), Func (tv, t))

            | IR.Application (f, a, data) ->
                let (as1, cs1, t1) = infer f mset
                let (as2, cs2, t2) = infer a mset
                let tv = fresh ()
                let newCs = EqConst (t1, (Func (t2, tv)))
                (Assumption.merge as1 as2, Seq.append (Seq.append cs1 cs2) [ newCs ], tv)

            | IR.Let l ->
                let x1 = l.value
                let x2 = l.body
                let (as1, cs1, t1) = infer x1 mset
                let (as2, cs2, t2) = infer x2 mset
                let x = AstCommon.getName l.binding
                let asms = Assumption.merge as1 (Assumption.remove as2 x)
                let newCs = Seq.map (fun ts -> ImpInstConst (ts, mset, t1)) (Assumption.lookup as2 x)
                (asms, Seq.append (Seq.append cs1 cs2) newCs, t2)

            | IR.LetRec _ -> invalidOp "Not implemented"

            | IR.Literal (lit, _) ->
                match lit with
                    | AstCommon.Float _ -> Assumption.empty, Seq.empty, Primitive Float
                    | AstCommon.Int _ -> Assumption.empty, Seq.empty, Primitive Int
                    | AstCommon.Bool _ -> Assumption.empty, Seq.empty, Primitive Bool
                    | AstCommon.String _ -> Assumption.empty, Seq.empty, Primitive String
                    | AstCommon.Unit -> Assumption.empty, Seq.empty, Primitive Unit
                    | AstCommon.List _ -> invalidOp "Not implemented"
            | IR.If (cond, ifExpr, elseExpr, data) ->
                let (as1, cs1, t1) = infer cond mset
                let (as2, cs2, t2) = infer ifExpr mset
                let (as3, cs3, t3) = infer elseExpr mset
                let newCs = seq [
                    EqConst (t1, (Primitive Bool))
                    EqConst (t2, t3)
                ]
                (Assumption.mergeMany [ as1; as2; as3 ], Seq.concat [ cs1; cs2; cs3; newCs ], t2)
    
    let inferType env (expr: IR.Expression<'data, AstCommon.Declaration>): Result<Substitute.Substitution * Type, TypeError> =
        let (a, cs, t) = infer expr Set.empty
        let unbounds = Set.difference (Set.ofList (Assumption.keys a)) (Set.ofSeq (Environment.keys env))
        match Set.isEmpty unbounds with
            | false -> UnboundVariable (Set.minElement unbounds) |> Error
            | true ->
                let externConstraints = seq {
                    let e = Environment.toSeq env
                    for (x, s) in e do
                        for t in (Assumption.lookup a x) do
                            yield ExpInstConst (t, s)
                }
                solveAll (Seq.append externConstraints cs) |> Result.bind (fun subst -> Ok (subst, Substitute.substType subst t))

    let inferExpr (env: Environment.Env) (expr: IR.Expression<'data, AstCommon.Declaration>) =
        match inferType env expr with
            | Error e -> Error e
            | Ok (subst, t) -> 
                printfn "%A" subst
                Ok (closeOver (Substitute.substType subst t))

    let rec toType (td: AstCommon.TypeDeclaration) =
        match td with
            | AstCommon.Name "Int" -> Primitive Int
            | AstCommon.Name "String" -> Primitive String
            | AstCommon.Name "Float" -> Primitive Float
            | AstCommon.Name "Bool" -> Primitive Bool
            | AstCommon.Name "Unit" -> Primitive Unit
            | AstCommon.Name _ -> fresh () //TODO
            | AstCommon.Generic _ -> fresh () //TODO
            | AstCommon.Function (itd, otd) -> Func (toType itd, toType otd)
            | AstCommon.Parameterized (name, parameters) ->
                Parameterized (name, List.map toType parameters)

    let initialContext = Environment.fromSeq (Seq.map (fun (n, td) -> n, toType td |> generalize Set.empty) (Map.toSeq initialTypes))

    inferExpr initialContext