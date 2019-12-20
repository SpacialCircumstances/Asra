module TypecheckerTests

open Xunit
open Typechecker
open Asserts
open System.Diagnostics

let testLog s = Debug.WriteLine s

type TestError =
    | ParserError of string
    | TypeError of TypeError<unit>

[<Fact>]
let ``id function`` () =
    Errors.result {
        let code = "fun a -> a"
        let! ast = Parser.testParser code |> Result.mapError ParserError
        let ir = IRGenerator.map ast
        let tc = Typechecker.createContext Map.empty testLog
        let! pt = tc ir |> Result.mapError TypeError
        return getType pt
    } |> assertEqResult (Func (Var "t0", Var "t0"))