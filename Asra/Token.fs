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
    | Do
    | Rec
    | End
    | If
    | Then
    | Else
    | Extern
    | Import
    | Match
    | With
    | Fun
    | Type
    | Equal
    | Arrow
    | VerticalBar
    | Separator
    | LeftParen
    | RightParen
    | LeftSquareBracket
    | RightSquareBracket
    | LeftCurlyBracket
    | RightCurlyBracket
    | Comma
    | Period
    | Colon

[<Struct>]
type Token = {
    data: TokenData
    position: SourcePosition
}