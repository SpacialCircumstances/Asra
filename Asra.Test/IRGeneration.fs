module IRGeneration

open Asserts
open AstCommon
open Xunit

[<Fact>]
let ``Lambda`` () =
    let ast = FrontendAst.Lambda([
        Named "a"
        Named "b"
        Named "c"
    ], FrontendAst.Variable("a", ()), ())
    let ir = IR.Lambda(Named "a", 
                IR.Lambda(Named "b", 
                    IR.Lambda(Named "c", 
                        IR.Variable ("a", ()), ()), ()), ())
    assertEq ir (IRGenerator.map ast)

[<Fact>]
let ``Let`` () =
    let ast = FrontendAst.Let([
        None, Named "x", FrontendAst.Literal (Int 2L, ())
        Some FrontendAst.Recursive, Named "d", FrontendAst.Lambda ([
            Named "a"
            Named "b"
        ], FrontendAst.Group(FrontendAst.Variable("a", ()), ()), ())
    ], FrontendAst.Variable("t", ()), ())
    let ir = IR.Let ({
        data = ()
        binding = Named "x"
        value = IR.Literal (Int 2L, ())
        body = IR.LetRec({
            data = ()
            binding = Named "d"
            value = IR.Lambda (Named "a", IR.Lambda(Named "b", IR.Variable("a", ()), ()), ())
            body = IR.Variable("t", ())
        })
    })
    assertEq ir (IRGenerator.map ast)
