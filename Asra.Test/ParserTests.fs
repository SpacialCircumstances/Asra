module ParserTests

open Asserts
open AstCommon
open FrontendAst
open Parser
open Xunit

let testCases () =
    [
        "12", Literal (Int 12L, ())
        "2188", Literal (Int 2188L, ())
        "-21", Literal (Int -21L, ())
        "002", Literal (Int 2L, ())
        "322.32", Literal (Float 322.32, ())
        "-123.0", Literal (Float -123.0, ())
        "()", Literal (Unit, ())
        "\"\"", Literal (String "", ())
        "\"test\"", Literal (String "test", ())
        "\"hello\\nworld\"", Literal (String "hello\nworld", ())
        "false", Literal (Bool false, ())
        "true", Literal (Bool true, ())
        "test", Variable ("test", ())
        "(  test)", Group (Variable ("test", ()), ())
        "(12)", Group (Literal (Int 12L, ()), ())
        "(())", Group (Literal (Unit, ()), ())
        "fun x y -> test", Lambda ([ Named "x"; Named "y" ], Variable ("test", ()), ())
        "fun a -> fun b -> b", Lambda (
            [ 
                Named "a"
            ],
            Lambda ([
                Named "b"
            ], Variable ("b", ()), ()), ())
        "fun t -> ()", Lambda ([ Named "t" ], Literal (Unit, ()), ())
        """
        let 
            x = fun y -> y
        in
            x
        end
        """, Let ([
            None, Named "x", Lambda ([ Named "y" ], Variable ("y", ()), ())
        ], Variable ("x", ()), ())
        """
        let a = 2 in
        let b = "test" in
        #asdf
        fun x -> a
        end
        end
        """, Let ([
            None, Named "a", Literal (Int 2L, ())
        ], Let([
            None, Named "b", Literal (String "test", ())
        ], Lambda ([ Named "x" ], Variable ("a", ()), ()), ()), ())
        """
        let
            w = 12
            id = fun x -> x #qwertz
            m = (fun y -> y)
        in
            w
        end
        """, Let ([
            None, Named "w", Literal (Int 12L, ())
            None, Named "id", Lambda ([ Named "x" ], Variable ("x", ()), ())
            None, Named "m", Group (Lambda ([ Named "y" ], Variable ("y", ()), ()), ())
        ], Variable ("w", ()), ())
        """
        let
            id = fun x -> x
            rec fac = fun i -> multiply (fac (substract i 1)) i
        in fac 3 end
        """, Let([
            None, Named "id", Lambda ([ Named "x" ], Variable ("x", ()), ())
            Some Recursive, Named "fac", Lambda ([
                Named "i"
            ], FunctionCall (Variable ("multiply", ()), [
                Group (FunctionCall (Variable ("fac", ()), [
                    Group (
                        FunctionCall (Variable ("substract", ()), [
                            Variable ("i", ())
                            Literal (Int 1L, ())
                        ], ())
                    , ()) ], ()), ());
                 Variable ("i", ()) ], ()), ())
        ], FunctionCall (Variable ("fac", ()), [ Literal (Int 3L, ()) ], ()), ())
        "let (x: Int) = 2 in x end", Let ([
            None, TypeAnnotated ("x", Name "Int"), Literal (Int 2L, ())
        ], Variable ("x", ()), ())
        "fun (x: String) -> x", Lambda([
            TypeAnnotated ("x", Name "String")
        ], Variable ("x", ()), ())
        "let (test: (Int)) = 4 in test end", Let ([
            None, TypeAnnotated ("test", Name "Int"), Literal (Int 4L, ())
        ], Variable ("test", ()), ())
        "fun (y: Int -> Int) -> y 2", Lambda([
            TypeAnnotated ("y", Function (Name "Int", Name "Int"))
        ], FunctionCall (Variable ("y", ()), [ Literal (Int 2L, ()) ], ()), ())
        "fun (x: 'a) -> \"test\"", Lambda([
            TypeAnnotated ("x", Generic "a")
        ], Literal (String "test", ()), ())
        "fun (x: 'a) (xs: List 'a) -> append xs x", Lambda([
            TypeAnnotated ("x", Generic "a")
            TypeAnnotated ("xs", Parameterized ("List", [
                Generic "a"
            ]))
        ], FunctionCall (Variable ("append", ()), [
                Variable ("xs", ())
                Variable ("x", ()) ], ()), ())
        "let (z: Map (List String) 'value) = newMap in z end", Let([
            None, TypeAnnotated ("z", Parameterized ("Map", [
                Parameterized ("List", [ Name "String" ])
                Generic "value"
            ])), Variable ("newMap", ())
        ], Variable("z", ()), ())
        "fun (a: (Int -> Int) -> String) (b: String -> 'a -> 'a) -> true", Lambda ([
            TypeAnnotated ("a", Function (Function (Name "Int", Name "Int"), Name "String"))
            TypeAnnotated ("b", Function (Name "String", (Function (Generic "a", Generic "a"))))
        ], Literal (Bool true, ()), ())
        "fun (a: (List Int) -> 'a) -> a", Lambda ([
            TypeAnnotated ("a", Function (Parameterized ("List", [
                Name "Int"
            ]), Generic "a"))
        ], Variable ("a", ()), ())
        "import Test", Import ("Test", ())
        """
        let
            #42
            x = 2
            test = import Test
        in test end
        """, Let ([
            None, Named "x", Literal (Int 2L, ())
            None, Named "test", Import ("Test", ())
        ], Variable ("test", ()), ())
        "if x then a else b end", If (Variable ("x", ()), Variable ("a", ()), Variable ("b", ()), ())
        """
        if true then
            stuff
        else () end
        """, If (Literal (Bool true, ()), Variable ("stuff", ()), Literal (Unit, ()), ())
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
                None, Named "a", FunctionCall (Variable ("f", ()), [
                    Variable ("x", ())
                    Variable ("y", ())
                ], ())
                None, Named "b", FunctionCall (Variable ("g", ()), [
                    Variable ("c", ())
                    Variable ("d", ())
                ], ())
            ],
            FunctionCall (Variable ("h", ()), [
                Variable ("a", ())
                Variable ("b", ())
            ], ()), ())
        "(+) 1 2", FunctionCall (Variable ("+", ()), [
            Literal (Int 1L, ())
            Literal (Int 2L, ())
        ], ())
        "- 2", UnaryOperatorCall ("-", Literal (Int 2L, ()), ())
        "-(2)", UnaryOperatorCall ("-", Group (Literal (Int 2L, ()), ()), ())
        "!false", UnaryOperatorCall ("!", Literal (Bool false, ()), ())
        "2 + 2", BinaryOperatorCall (Literal (Int 2L, ()), "+", Literal (Int 2L, ()), ())
        "1 + 2 + 3", BinaryOperatorCall (Literal (Int 1L, ()), "+", BinaryOperatorCall (Literal (Int 2L, ()), "+", Literal (Int 3L, ()), ()), ())
        "(1 * 2) + (3 - 4)", BinaryOperatorCall (
            Group (BinaryOperatorCall (Literal (Int 1L, ()), "*", Literal (Int 2L, ()), ()), ()),
            "+",
            Group (BinaryOperatorCall (Literal (Int 3L, ()), "-", Literal (Int 4L, ()), ()), ()),
            ())
        """
        let 
            ((+): Int -> Int) = fun a b -> plus a b
        in
            2 + -12
        end
        """, Let ([
            None, TypeAnnotated ("+", Function (Name "Int", Name "Int")), Lambda ([
                Named "a"
                Named "b"
            ], FunctionCall (Variable ("plus", ()), [
                Variable ("a", ())
                Variable ("b", ())], ()), ())
        ], BinaryOperatorCall (Literal (Int 2L, ()), "+", Literal (Int -12L, ()), ()), ())
        "[ 1; 2; 3; 4]", Literal (List [
            Literal (Int 1L, ())
            Literal (Int 2L, ())
            Literal (Int 3L, ())
            Literal (Int 4L, ())
        ], ())
        "[]", Literal (List [], ())
        """
        [
            "Test"
            value
            (f x)
            []
        ]
        """, Literal (List [
            Literal (String "Test", ())
            Variable ("value", ())
            Group (FunctionCall (Variable ("f", ()), [ Variable("x", ()) ], ()), ())
            Literal (List [], ())
        ], ())
    ] |> Seq.map (fun (code, ast) -> [| box code; box ast|])

[<Theory>]
[<MemberData("testCases")>]
let ``Parse expressions`` (code: string, ast: Expression<unit>) =
    assertEqResult ast (testParser code)