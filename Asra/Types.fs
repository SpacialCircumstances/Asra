module Types

type Primitive =
    | Int
    | Float
    | Bool
    | String
    | Unit

[<StructuredFormatDisplay("{AsString}")>]
type AType =
    | Primitive of Primitive
    | Func of AType * AType
    | Var of string
    | Parameterized of string * AType list
with
    override self.ToString () =
        let parameterizedToString (name, types: AType list) = sprintf "%s %s" name (System.String.Join(" ", types))
        match self with
            | Parameterized (name, types) -> parameterizedToString (name, types)
            | Primitive str -> str.ToString ()
            | Var tp -> "'" + tp
            | Func (input, output) -> 
                match input with
                    | Var tp ->
                        sprintf "'%s -> %O" tp output
                    | Primitive tp ->
                        sprintf "%O -> %O" tp output
                    | Parameterized (name, types) ->
                        sprintf "(%s) -> %O" (parameterizedToString (name, types)) output
                    | Func (fti, fto) ->
                        sprintf "(%O -> %O) -> %O" fti fto output
    member self.AsString = self.ToString()