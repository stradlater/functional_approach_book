module Tests

open System
open Xunit
open Chapter5


[<Fact>]
let ``Test ofString`` () =

    let treeExpr = "ash(bas, c(foo, bar), d, d)"

    let gentree = GenTree.ofString id treeExpr
    
    let (GenNode(a, rest)) =  gentree
    Assert.Equal("ash", a);

    let size = GenTree.size gentree
    Assert.Equal(7,size)

    let set = GenTree.set gentree
    Assert.Equal(6, set.Count)
    
[<Fact>]
let ``Test ofString 2`` () =
    let treeExpr = "ash(bas, d)"

    let gentree = GenTree.ofString id treeExpr
    
    let (GenNode(a, [GenNode(b, []); GenNode(c, [])] )) =  gentree
    Assert.Equal("ash", a);
    Assert.Equal("bas", b);
    Assert.Equal("d", c);

    
[<Fact>]
let ``Test ofString 3`` () =
    let treeExpr = "ash(bas, d(a,b,c), d, e(f, g(h)))"

    let gentree = GenTree.ofString id treeExpr
    
    Assert.Equal(11, GenTree.size gentree)


[<Fact>]
let ``Test ofString 4`` () =
    let treeExpr = "ash(bas,d(a,b,c),d(asd,dsdsd,asd(lk(asd(asd)),df,df)),e(f,g(h)))"
    let gentree = GenTree.ofString id treeExpr
    let str = GenTree.toString id gentree
    Assert.Equal(treeExpr, str)

    
[<Fact>]
let ``Test ofString 5`` () =
    let treeExpr = "ash"
    let gentree = GenTree.ofString id treeExpr
    let str = GenTree.toString id gentree
    Assert.Equal(treeExpr, str)