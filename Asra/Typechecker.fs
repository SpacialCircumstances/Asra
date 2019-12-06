﻿module Typechecker

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
    
type Scheme = (Var list) * Type

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

    let mergeMany assumptions = { assumptions = List.concat (List.map (fun a -> a.assumptions) assumptions) }

    let singleton (name, tp) = { assumptions = List.singleton (name, tp) }

    let keys assumption = List.map fst assumption.assumptions

type Constraint =
    | EqConst of Type * Type
    | ExpInstConst of Type * Scheme
    | ImpInstConst of Type * Set<Var> * Scheme

type TypeError =
    | UnificationFail of Type * Type
    | InfiniteType of Var * Type
    | UnboundVariable of string
    | Ambigious of Constraint list
    | UnificationMismatch of Type list * Type list

module Substitute =
    type Substitution = Map<Var, Type>
    
    type Substitute<'a> = Substitution -> 'a -> 'a

    let substMany = fun s (l: 'a list) (f: Substitute<'a>) -> List.map (f s) l

    let substSet = fun s (set: Set<'a>) (f: Substitute<'a>) -> Set.map (f s) set

    let substVar: Substitute<Var> = fun s a -> match Map.tryFind a s with
                                                | Some (Var v) -> v
                                                | _ -> a

    let rec substType: Substitute<Type> = fun s t ->
        match t with
            | Primitive a -> Primitive a
            | Var v -> substVar s v |> Var
            | Func (t1, t2) -> Func (substType s t1, substType s t2)
            | Parameterized _ -> invalidOp "Not implemented"

    let substScheme: Substitute<Scheme> = fun s scheme ->
        let (ts, t) = scheme
        ts, substType (List.foldBack Map.remove ts s) t

    let substConstraint: Substitute<Constraint> = fun s c ->
        match c with
            | EqConst (t1, t2) -> EqConst (substType s t1, substType s t2)
            | ExpInstConst (t1, t2) -> ExpInstConst (substType s t1, substScheme s t2)
            | ImpInstConst (t1, m, t2) -> ImpInstConst (substType s t1, substSet s m substVar, substScheme s t2)

module TypeVars =
    type FreeTypeVars<'a> = 'a -> Set<Var>

    type ActiveTypeVars<'a> = 'a -> Set<Var>

    let freeVar: FreeTypeVars<Var> = Set.singleton

    let freeMany (l: List<'a>) (f: FreeTypeVars<'a>) = Set.unionMany (List.map f l)

    let freeSet (s: Set<'a>) (f: FreeTypeVars<'a>) = Set.unionMany (Seq.map f (Set.toSeq s))

    let rec freeType: FreeTypeVars<Type> = fun t ->
        match t with
            | Primitive _ -> Set.empty
            | Var a -> Set.singleton a
            | Func (t1, t2) -> Set.union (freeType t1) (freeType t2)
            | Parameterized _ -> invalidOp "Not implemented"

    let freeScheme: FreeTypeVars<Scheme> = fun (ts, t) -> Set.difference (freeType t) (Set.ofList ts)

    let activeConstraint: ActiveTypeVars<Constraint> = fun c ->
        match c with
            | EqConst (t1, t2) -> Set.union (freeType t1) (freeType t2)
            | ExpInstConst (t, s) -> Set.union (freeType t) (freeScheme s)
            | ImpInstConst (t1, m, t2) -> Set.union (freeType t1) (Set.intersect (freeSet m freeVar) (freeScheme t2))

    let activeMany (l: List<'a>) (f: ActiveTypeVars<'a>) = Set.unionMany (Seq.map f l)

let createContext (initialTypes: Map<string, AstCommon.TypeDeclaration>) =
    let varNameCounter = ref 0
    
    let nextName () = 
        let x = sprintf "t%i" !varNameCounter
        incr varNameCounter
        x

    let fresh () = nextName () |> Var
    
    let solve cs = Map.empty

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
                let addAsms = Seq.map (fun ts -> EqConst (ts, tv)) (Assumption.lookup asm name)
                (Assumption.remove asm (AstCommon.getName d), (Seq.append cs addAsms), Func (tv, t))

            | IR.Application (f, a, data) ->
                let (as1, cs1, t1) = infer f mset
                let (as2, cs2, t2) = infer a mset
                let tv = fresh ()
                let newAs = EqConst (t1, (Func (t2, tv)))
                (Assumption.merge as1 as2, Seq.append (Seq.append cs1 cs2) [ newAs ], tv)
    
    let inferType env (expr: IR.Expression<'data, AstCommon.Declaration>): Result<Substitute.Substitution * Type, TypeError> =
        let (a, cs, t) = infer expr Set.empty
        let unbounds = Set.difference (Set.ofList (Assumption.keys a)) (Set.ofSeq (Environment.keys env))
        match Set.isEmpty unbounds with
            | true -> UnboundVariable (Set.minElement unbounds) |> Error
            | false ->
                let cs2 = seq {
                    let e = Environment.toSeq env
                    for (x, s) in e do
                        for t in (Assumption.lookup a x) do
                            yield ExpInstConst (t, s)
                }
                let subst = solve (Seq.append cs cs2)
                Ok (subst, Substitute.substType subst t)
    
    let generalize (vars: Set<Var>) (t: Type): Scheme =
        let ts = Set.toList (Set.difference (TypeVars.freeType t) vars)
        (ts, t)

    let normalize = id //TODO

    let closeOver (t: Type): Scheme = generalize Set.empty t |> normalize

    let inferExpr (env: Environment.Env) (expr: IR.Expression<'data, AstCommon.Declaration>) =
        match inferType env expr with
            | Error e -> Error e
            | Ok (subst, t) -> Ok (closeOver (Substitute.substType subst t))

    inferExpr