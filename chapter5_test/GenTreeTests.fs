module GenTreeTests

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

[<Fact>]
let ``Test flat`` () =
    let treeExpr = "a(b,c(d,e),f)"
    let gentree = GenTree.ofString id treeExpr
    let flist = GenTree.flat gentree
    let flatString = List.fold (+) "" flist
    Assert.Equal("abcdef", flatString)



[<Fact>]
let ``Test at`` () =
    let treeExpr = "a(b,c(d,e),f)"
    let gentree = GenTree.ofString id treeExpr
    Assert.Equal(treeExpr, (GenTree.at gentree []) |> (GenTree.toString id))

    Assert.Equal("b", (GenTree.at gentree [0]) |> (GenTree.toString id))
    Assert.Equal("c(d,e)", (GenTree.at gentree [1]) |> (GenTree.toString id))
    Assert.Equal("f", (GenTree.at gentree [2]) |> (GenTree.toString id))
    Assert.Equal("d", (GenTree.at gentree [1; 0]) |> (GenTree.toString id))
    Assert.Equal("e", (GenTree.at gentree [1; 1]) |> (GenTree.toString id))


[<Fact>]
let ``Test replace`` () =
    let treeExpr = "a(b,c(d,e),f)"
    let gentree = GenTree.ofString id treeExpr

    let repExpr = "foo(a,b,c)"
    let repTree = GenTree.ofString id repExpr

    Assert.Equal(repExpr, (GenTree.replaceOcc [] repTree gentree) |> (GenTree.toString id))
    Assert.Equal("a(foo(a,b,c),c(d,e),f)", (GenTree.replaceOcc [0] repTree gentree) |> (GenTree.toString id))
    Assert.Equal("a(b,c(d,foo(a,b,c)),f)", (GenTree.replaceOcc [1;1] repTree gentree) |> (GenTree.toString id))