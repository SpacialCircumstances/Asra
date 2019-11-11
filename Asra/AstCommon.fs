module AstCommon

[<StructuredFormatDisplay("{AsString}")>]
type Literal<'expr> =
    | Int of int64
    | String of string
    | Float of float
    | Bool of bool
    | Unit
    | List of 'expr list
with
    override self.ToString () =
        match self with 
            | Int i -> sprintf "%iI" i
            | Float f -> sprintf "%fF" f
            | Bool b -> sprintf "%b" b
            | Unit -> "()"
            | String s -> s
            | List exprs -> sprintf "[%s]" (System.String.Join("; ", exprs))
    member self.AsString = self.ToString ()

[<StructuredFormatDisplay("{AsString}")>]
type TypeDeclaration =
    | Name of string
    | Generic of string
    | Parameterized of string * TypeDeclaration list
    | Function of TypeDeclaration * TypeDeclaration
with
    override self.ToString () =
        let parameterizedToString (name, types: TypeDeclaration list) = sprintf "%s %s" name (System.String.Join(" ", types))
        match self with
            | Parameterized (name, types) -> parameterizedToString (name, types)
            | Name str -> str.ToString ()
            | Generic tp -> "'" + tp
            | Function (input, output) -> 
                match input with
                    | Generic tp ->
                        sprintf "'%s -> %O" tp output
                    | Name tp ->
                        sprintf "%O -> %O" tp output
                    | Parameterized (name, types) ->
                        sprintf "(%s) -> %O" (parameterizedToString (name, types)) output
                    | Function (fti, fto) ->
                        sprintf "(%O -> %O) -> %O" fti fto output
    member self.AsString = self.ToString ()

[<StructuredFormatDisplay("{AsString}")>]
type Declaration =
    | Named of string
    | TypeAnnotated of string * TypeDeclaration
with
    override self.ToString () =
        match self with
            | Named n -> n
            | TypeAnnotated (name, td) -> sprintf "(%s: %A)" name td
    member self.AsString = self.ToString ()