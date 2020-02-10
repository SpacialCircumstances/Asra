module Lexer

open Token

type NumberLiteralState =
    | BeforeDecimalPoint
    | AfterDecimalPoint

type CurrentTokenType =
    | NumberLiteral of NumberLiteralState
    | StringLiteral
    | Identifier
    | Comment

type State = 
    | NextToken
    | Current of SourcePosition * CurrentTokenType

type LexerError = {
    start: SourcePosition option
    error: SourcePosition
    message: string
    token: CurrentTokenType option
}

let separatorSet = set [
    ' '
    '\n'
    '\r'
    '\t'
    '"'
    ':'
    ';'
    '('
    ')'
    '{'
    '}'
    '['
    ']'
    '#'
]

let isSeparator c = Set.contains c separatorSet

let keywordMap = Map.ofList [
    "let", Let
    "in", In
    "rec", Rec
    "end", End
    "if", If
    "then", Then
    "else", Else
    "fun", Fun
    "type", Type
    "->", Arrow
]

let handleKeywords (identifier: string) = 
    match Map.tryFind identifier keywordMap with
        | None -> TokenData.Identifier identifier
        | Some kw -> kw

let lexer (name: string) (code: string) =
    let tokens = ResizeArray<Token> ()
    let errors = ResizeArray<LexerError> ()

    let mutable pos = 0
    let mutable line = 1
    let mutable col = 0
    let mutable state = NextToken

    let currentPos () = {
        line = line
        col = col
        position = pos
        filename = name
    }

    let error msg = 
        let e = match state with
                    | NextToken ->
                        {
                            message = msg
                            token = None
                            start = None
                            error = currentPos ()
                        }
                    | Current (start, ct) ->
                        {
                            message = msg
                            token = Some ct
                            start = Some start
                            error = currentPos ()
                        }
        do errors.Add e
        state <- NextToken

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
        position = currentPos ()
    }

    let setCurrentToken tt =
        state <- Current (currentPos (), tt)

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
                        setCurrentToken (NumberLiteral BeforeDecimalPoint)
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
                    | NumberLiteral numberState ->
                        match current with
                            | '.' -> match numberState with
                                        | BeforeDecimalPoint ->
                                            state <- Current (tokenStart, NumberLiteral AfterDecimalPoint)
                                            incrp ()
                                        | AfterDecimalPoint -> error "Encountered multiple decimal points"
                            | _ ->
                                if System.Char.IsDigit current then 
                                    incrp ()
                                else if isSeparator current then
                                    let lit = code.[tokenStart.position..(pos-1)]
                                    let token = {
                                        data = TokenData.NumberLiteral lit
                                        position = tokenStart
                                    }
                                    addToken token
                                else
                                    sprintf "Encountered invalid character: %c" current |> error
                    | Identifier -> 
                        if isSeparator current then
                            let identifier = code.[tokenStart.position..(pos-1)]
                            let token = {
                                data = handleKeywords identifier
                                position = tokenStart
                            }
                            addToken token
                        else incrp ()

    match state with
        | Current (_, StringLiteral) -> error "Unterminated string literal"
        | _ -> ()

    tokens, errors