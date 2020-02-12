module LexerTests

open Xunit

let tokenizedMatch code tokenData =
    let t, e = Lexer.lexer "Test" code
    Assert.Empty(e)
    let data = Seq.map (fun (t: Token.Token) -> t.data) t |> Seq.toList
    Assert.Equal<Token.TokenData list>(tokenData, data)

