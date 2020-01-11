module Types

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

type DataWithType<'data> = {
    nodeData: 'data
    nodeType: Scheme
}

let getExprType (expr: IR.Expression<DataWithType<'data>>) = (IR.getData expr).nodeType