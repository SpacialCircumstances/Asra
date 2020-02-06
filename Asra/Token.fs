module Token

[<StructuredFormatDisplay("{filename}:{line}:{col}")>]
type SourcePosition = {
    line: int
    col: int
    position: int
    filename: string
}

type TokenData =
    | StringLiteral of string
    | NumberLiteral of string
    | Identifier of string
    | Let
    | In
    | Rec
    | End
    | If
    | Then
    | Else
    | Fun
    | Type
    | Equal
    | Arrow
    | LeftParen
    | RightParen
    | LeftSquareBracket
    | RightSquareBracket
    | LeftCurlyBracket
    | RightCurlyBracket
    | Semicolon
    | Comma
    | Period
    | Colon

[<Struct>]
type Token = {
    data: TokenData
    position: SourcePosition
}