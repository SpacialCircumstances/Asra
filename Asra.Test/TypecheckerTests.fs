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
        let tc = Typechecker.createContext Prelude.context testLog
        let! pt = tc ir |> Result.mapError TypeError
        return normalize (getExprType pt)
    } |> assertEqResult expectedType

[<Fact>]
let ``id function`` () = typecheck (Scheme (["t0"], Func (Var "t0", Var "t0"))) "fun a -> a"

[<Fact>]
let ``let id function`` () = typecheck (Scheme (["t0"], Func (Var "t0", Var "t0"))) "let id = fun a -> a in id end"

[<Fact>]
let ``generalized id function`` () =
    do typecheck 
        (Scheme ([], Primitive Int))
        """
        let
            id = fun a -> a
            a = id "Test"
            b = id 4
        in b end
        """
        
[<Fact>]
let ``id with let`` () =
    typecheck (Scheme (["t0"], Func (Var "t0", Var "t0"))) "fun x -> let y = x in y end"

[<Fact>]
let ``wrap function`` () =
    typecheck (Scheme (["t0"; "t1"], Func (Func (Var "t0", Var "t1"), Func (Var "t0", Var "t1")))) "fun x -> let y = fun z -> (x z) in y end"

[<Fact>]
let ``List typing 1`` () =
    typecheck (Scheme (["t0"], Parameterized ("List", [Var "t0"]))), "[]"

[<Fact>]
let ``List typing 2`` () =
    typecheck (Scheme ([], Parameterized ("List", [Primitive Int]))), "[12; 23]"

[<Fact>]
let ``List typing 3`` () =
    typecheck (Scheme ([], Parameterized ("List", [Primitive Float]))), "let x = 2.3 .+ 4.3 in [x] end"

[<Fact>]
let ``Variable shadowing 1`` () =
    do typecheck
        (Scheme ([], Primitive Int))
        """
        let
            a = "Test"
            a = 2
        in a end
        """

[<Fact>]
let ``Variable shadowing 2`` () =
    do typecheck
        (Scheme ([], Func (Primitive Int, Primitive Int)))
        """
        let
            a = "Test"
            succ = fun a -> a + 1
        in succ end
        """