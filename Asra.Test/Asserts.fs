module Asserts

open Xunit

let assertEqResult (expected: 'a) (got: Result<'a, 'b>) =
    let gotR = match got with
                    | Error e -> 
                        Assert.True(false, e.ToString())
                        invalidOp "Unreachable"
                    | Ok g -> g
    Assert.Equal<'a>(expected, gotR)

let assertEq (expected: 'a) (got: 'a) = Assert.Equal<'a>(expected, got)