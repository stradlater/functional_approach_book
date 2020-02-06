module Tests

open System
open Xunit
open Chapter5


[<Fact>]
let ``My test`` () =

    let treeExpr = "asa(bas, c(foo, bar), d, d)"

    let gentree = GenTree.ofString id treeExpr

    let size = GenTree.size gentree
    Assert.Equal(7,size)

    let set = GenTree.set gentree
    Assert.Equal(6, set.Count)
    