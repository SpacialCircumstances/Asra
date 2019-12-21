module TypecheckerTests

open Xunit
open Typechecker
open Asserts
open System.Diagnostics

let testLog s = Debug.WriteLine s

type TestError =
    | ParserError of string
    | TypeError of TypeError<unit>

let typecheck (expectedType: Scheme) (code: string) = 
    Errors.result {
        let! ast = Parser.testParser code |> Result.mapError ParserError
        let ir = IRGenerator.map ast
        let tc = Typechecker.createContext Map.empty testLog
        let! pt = tc ir |> Result.mapError TypeError
        return getExprType pt
    } |> assertEqResult expectedType

[<Fact>]
let ``id function`` () = typecheck (Scheme (["t0"], Func (Var "t0", Var "t0"))) "fun a -> a"