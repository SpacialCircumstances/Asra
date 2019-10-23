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
        "false", Bool false
        "true", Bool true
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
        ], Variable ("w", ()), ())
    ] |>
    List.iter (fun (code, expectedAst) ->
        let res = testParser code
        assertEqResult expectedAst res
    )

[<Fact>]
let ``Import parsing`` () =
    [
        "import! Test", Import ("Test", ())
        """
        let
            x = 2
            test = import! Test
        in test end
        """, Let ([
            Named "x", Literal (Int 2L, ())
            Named "test", Import ("Test", ())
        ], Variable ("test", ()), ())
    ] |>
    List.iter (fun (code, expectedAst) ->
        let res = testParser code
        assertEqResult expectedAst res
    )

[<Fact>]
let ``If parsing`` () =
    [
        "if x then a else b end", If (Variable ("x", ()), Variable ("a", ()), Variable ("b", ()), ())
        """
        if true then
            stuff
        else () end
        """, If (Literal (Bool true, ()), Variable ("stuff", ()), Literal (Unit, ()), ())
    ] |>
    List.iter (fun (code, expectedAst) ->
        let res = testParser code
        assertEqResult expectedAst res
    )

[<Fact>]
let ``Function call parsing`` () =
    [
        "f x", FunctionCall (Variable ("f", ()), [
            Variable ("x", ())
        ], ())
        "f x y", FunctionCall (Variable("f", ()), [
            Variable ("x", ())
            Variable ("y", ())
        ], ())
        "f (x y)", FunctionCall (Variable ("f", ()), [
            Group (
                FunctionCall (Variable ("x", ()), [
                    Variable ("y", ())
                ], ()), ())
        ], ())
        "(f x) (g y)", FunctionCall (
            Group (
                FunctionCall (Variable ("f", ()), [
                    Variable ("x", ())
                ], ()), ()),
            [
                Group (
                    FunctionCall (Variable ("g", ()), [
                        Variable ("y", ())
                    ], ()), ())
            ],
            ())
        "(fun x -> x) y", FunctionCall (
            Group (
                Lambda ([
                    Named "x"
                ], Variable ("x", ()), ()),
                ()),
            [
                Variable ("y", ())
            ], ())
        """
        let 
            a = f x y
            b = g c d
        in 
            h a b
        end
        """, 
        Let ([
                Named "a", FunctionCall (Variable ("f", ()), [
                    Variable ("x", ())
                    Variable ("y", ())
                ], ())
                Named "b", FunctionCall (Variable ("g", ()), [
                    Variable ("c", ())
                    Variable ("d", ())
                ], ())
            ],
            FunctionCall (Variable ("h", ()), [
                Variable ("a", ())
                Variable ("b", ())
            ], ()), ())
    ] |>
    List.iter (fun (code, expectedAst) ->
        let res = testParser code
        assertEqResult expectedAst res
    )