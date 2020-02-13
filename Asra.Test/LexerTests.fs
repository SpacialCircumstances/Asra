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
        "[a; 22; 7 = 4]", [ LeftSquareBracket; Identifier "a"; Separator; NumberLiteral "22"; Separator; NumberLiteral "7"; Equal; NumberLiteral "4"; RightSquareBracket ]
        "{{ (= ..= ==; }", [ LeftCurlyBracket; LeftCurlyBracket; LeftParen; Equal; Identifier "..="; Identifier "=="; Separator; RightCurlyBracket ]
        "", []
        "fun let -> else if then if2 type", [ Fun; Let; Arrow; Else; If; Then; Identifier "if2"; Type ]
        """
        "Test", "sdf", 23, ad
        """, [ Separator; StringLiteral "Test"; Comma; StringLiteral "sdf"; Comma; NumberLiteral "23"; Comma; Identifier "ad"; Separator ]
    ] |> Seq.map (fun (code, ast) -> [| box code; box ast|])

let tokenizedMatch code tokenData =
    let t, e = Lexer.lexer "Test" code
    Assert.Empty(e)
    let data = Seq.map (fun (t: Token) -> t.data) t |> Seq.toList
    Assert.Equal<TokenData list>(tokenData, data)

[<Theory>]
[<MemberData("testCases")>]
let ``Tokenize`` (code: string, tokens: TokenData list) = do tokenizedMatch code tokens