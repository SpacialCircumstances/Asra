module Lexer

open Token

type CurrentTokenType =
    | NumberLiteral
    | StringLiteral
    | Identifier
    | Comment

type State = 
    | NextToken
    | Current of SourcePosition * CurrentTokenType

let lexer (name: string) (code: string) =
    let tokens = ResizeArray<Token> ()
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
            position = pos
            filename = name
        }
    }

    let setCurrentToken tt =
        let pos = {
            line = line
            col = col
            position = pos
            filename = name
        }
        state <- Current (pos, tt)

    let singleCharToken c = match c with
                                | '(' -> ValueSome LeftParen
                                | ')' -> ValueSome RightParen
                                | '{' -> ValueSome LeftCurlyBracket
                                | '}' -> ValueSome RightCurlyBracket
                                | '[' -> ValueSome LeftSquareBracket
                                | ']' -> ValueSome RightSquareBracket
                                | '=' -> ValueSome Equal
                                | ';' -> ValueSome Semicolon
                                | ':' -> ValueSome Colon
                                | ',' -> ValueSome Comma
                                | _ -> ValueNone

    while pos > (String.length code) do
        let current = code.[pos]
        match state with
            | NextToken ->
                match current with
                    | ' ' | '\t' | '\r' -> incrp ()
                    | '#' -> 
                        setCurrentToken Comment
                        incrp ()

                    | '\n' -> incrl ()
                    | '"' ->
                        setCurrentToken StringLiteral
                        incrp ()
                    | c when System.Char.IsDigit c ->
                        setCurrentToken NumberLiteral
                        incrp ()
                    | c -> match singleCharToken c with
                            | ValueSome td -> 
                                token td |> addToken
                                incrp ()
                            | ValueNone ->
                                setCurrentToken Identifier
                                incrp ()

            | Current (tokenStart, token) -> 
                match token with
                    | Comment ->
                        match current with
                            | '\n' ->
                                state <- NextToken
                                incrl ()
                            | _ -> incrp ()
                    | StringLiteral ->
                        //TODO: Support unicode, escaping and other fancy stuff
                        if current = '"' then
                            let lit = code.[(tokenStart.position + 1) .. (pos - 1)]
                            let token = {
                                data = TokenData.StringLiteral lit
                                position = tokenStart
                            }
                            addToken token
                            incrp ()
                        else incrp ()
                        ()
                    | _ -> ()
        ()

    tokens