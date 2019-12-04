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

let createContext (initialTypes: Map<string, AstCommon.TypeDeclaration>) =
    ()