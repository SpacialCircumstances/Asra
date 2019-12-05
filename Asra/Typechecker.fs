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

    let substVar (s: Substitution) (a: Var) = match Map.tryFind a s with
                                                | Some (Var v) -> v
                                                | _ -> a

    let rec substType (s: Substitution) (t: Type) =
        match t with
            | Primitive a -> Primitive a
            | Var v -> substVar s v |> Var
            | Func (t1, t2) -> Func (substType s t1, substType s t2)
            | Parameterized _ -> invalidOp "Not implemented"

    let substScheme (s: Substitution) (scheme: Scheme) =
        let (ts, t) = scheme
        ts, substType (List.foldBack Map.remove ts s) t

    let substConstraint (s: Substitution) (c: Constraint) =
        match c with
            | EqConst (t1, t2) -> EqConst (substType s t1, substType s t2)
            | ExpInstConst (t1, t2) -> ExpInstConst (substType s t1, substScheme s t2)
            | ImpInstConst (t1, m, t2) -> ImpInstConst (substType s t1, Set.map (substVar s) m, substScheme s t2)

let createContext (initialTypes: Map<string, AstCommon.TypeDeclaration>) =
    ()