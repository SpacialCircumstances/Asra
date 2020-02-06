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
    let state = NextToken

    while pos > (String.length code) do
        let current = code.[pos]
        match state with
            | NextToken ->
                match current with
                    | ' ' | '\t' | '\r' ->
                        pos <- pos + 1
                        col <- col + 1
                    | '\n' ->
                        pos <- pos + 1
                        col <- 0
                        line <- line + 1
                ()
            | Current token -> ()
        ()

    tokens