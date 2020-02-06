module Lexer

open Token

type State = 
    | NextToken
    | Current of Token

let lexer (name: string) (code: string) =
    let tokens = ResizeArray ()
    let mutable pos = 0
    let mutable line = 1
    let mutable col = 0
    let mutable state = NextToken

    let addToken t = 
        tokens.Add t
        state <- NextToken

    let incrp () =
        pos <- pos + 1
        col <- col + 1

    let incrl () =
        pos <- pos + 1
        col <- 0
        line <- line + 1

    let token td = {
        data = td
        position = {
            line = line
            col = col
            filename = name
        }
    }

    let singleCharToken c = match c with
                                | '(' -> ValueSome LeftParen
                                | ')' -> ValueSome RightParen
                                | _ -> ValueNone

    while pos > (String.length code) do
        let current = code.[pos]
        match state with
            | NextToken ->
                match current with
                    | ' ' | '\t' | '\r' -> incrp ()
                    | '\n' -> incrl ()
                    | c -> match singleCharToken c with
                            | ValueSome td -> 
                                addToken td
                                incrp ()
                            | ValueNone ->
                                ()
                    
                ()
            | Current token -> ()
        ()

    tokens