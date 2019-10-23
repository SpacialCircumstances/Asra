module ParserTests

open Ast
open Parser
open Xunit

let assertEqResult (expected: 'a) (got: Result<'a, 'b>) =
    let gotR = match got with
                    | Error e -> 
                        Assert.True(false, e.ToString())
                        invalidOp "Unreachable"
                    | Ok g -> g
    Assert.Equal<'a>(expected, gotR)

[<Fact>]
let ``Literal parsing`` () =
    [
        "12", Int 12L
        "2188", Int 2188L
        "-21", Int -21L
        "002", Int 2L
        "322.32", Float 322.32
        "-123.0", Float -123.0
        "()", Unit
        "\"\"", String ""
        "\"test\"", String "test"
        "\"hello\\nworld\"", String "hello\nworld"
    ] |>
    List.iter (fun (code, expectedAst) ->
        let res = testParser code
        assertEqResult (Literal (expectedAst, ())) res
    )

[<Fact>]
let ``Variable and group parsing`` () =
    [
        "test", Variable ("test", ())
        "+", Variable ("+", ())
        "(test)", Group (Variable ("test", ()), ())
        "(12)", Group (Literal (Int 12L, ()), ())
        "(())", Group (Literal (Unit, ()), ())
    ] |>
    List.iter (fun (code, expectedAst) ->
        let res = testParser code
        assertEqResult expectedAst res
    )

[<Fact>]
let ``Lambda parsing`` () =
    [
        "fun x y -> test", Lambda ([ Named "x"; Named "y" ], Variable ("test", ()), ())
        "fun a -> fun b -> b", Lambda (
            [ 
                Named "a"
            ],
            Lambda ([
                Named "b"
            ], Variable ("b", ()), ()), ())
        "fun t -> ()", Lambda ([ Named "t" ], Literal (Unit, ()), ())
    ] |>
    List.iter (fun (code, expectedAst) ->
        let res = testParser code
        assertEqResult expectedAst res
    )

[<Fact>]
let ``Let parsing`` () =
    [
        """
        let 
            x = fun y -> y
        in
            x
        end
        """, Let ([
            Named "x", Lambda ([ Named "y" ], Variable ("y", ()), ())
        ], Variable ("x", ()), ())
        """
        let a = 2 in
        let b = "test" in
        fun x -> a
        end
        end
        """, Let ([
            Named "a", Literal (Int 2L, ())
        ], Let([
            Named "b", Literal (String "test", ())
        ], Lambda ([ Named "x" ], Variable ("a", ()), ()), ()), ())
        """
        let
            w = 12
            id = fun x -> x
            m = (fun y -> y)
        in
            w
        end
        """, Let ([
            Named "w", Literal (Int 12L, ())
            Named "id", Lambda ([ Named "x" ], Variable ("x", ()), ())
            Named "m", Group (Lambda ([ Named "y" ], Variable ("y", ()), ()), ())
        ], Variable ("x", ()), ())
    ] |>
    List.iter (fun (code, expectedAst) ->
        let res = testParser code
        assertEqResult expectedAst res
    )