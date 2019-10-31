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