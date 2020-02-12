module LexerTests

open Xunit
open Token

let testCases () = 
    [
        "2 + 2", [ NumberLiteral "2"; Identifier "+"; NumberLiteral "2" ]
        "a; b; c++", [ Identifier "a"; Separator; Identifier "b"; Separator; Identifier "c++" ]
        "a b c++", [ Identifier "a"; Identifier "b"; Identifier "c++" ]
        "13.2 33.1 34", [ NumberLiteral "13.2"; NumberLiteral "33.1"; NumberLiteral "34" ]
        "(a, b)", [ LeftParen; Identifier "a"; Comma; Identifier "b"; RightParen ]
    ] |> Seq.map (fun (code, ast) -> [| box code; box ast|])

let tokenizedMatch code tokenData =
    let t, e = Lexer.lexer "Test" code
    Assert.Empty(e)
    let data = Seq.map (fun (t: Token) -> t.data) t |> Seq.toList
    Assert.Equal<TokenData list>(tokenData, data)

[<Theory>]
[<MemberData("testCases")>]
let ``Tokenize`` (code: string, tokens: TokenData list) = do tokenizedMatch code tokens