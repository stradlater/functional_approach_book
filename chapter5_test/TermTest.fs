module TermTests

open System
open Xunit
open Chapter5


[<Fact>]
let ``Test ofString`` () =
    let termString = "a(b,c())"
    let term = Term.ofString termString
    Assert.Equal(termString, Term.toString term)

[<Fact>]
let ``Test ofString 2`` () =
    let termString = "a(b(),c)"
    let term = Term.ofString termString
    Assert.Equal(termString, Term.toString term)

    
[<Fact>]
let ``Test vars`` () =
    let termString = "a(b(),c(c,d))"
    let term = Term.ofString termString
    let vars = Term.vars term
    let expectedVars = Set.ofList(["c";"d"])
    Assert.True( (Set.isSubset vars expectedVars) && (Set.isSubset expectedVars vars))

[<Fact>]
let ``Test Substitution`` () =
    let subst = Map.ofList [("x", Term.ofString "g(z)")]
    let substResult = Substitution.apply subst (Term.ofString "f(x,y,z)")
    Assert.Equal("f(g(z),y,z)", Term.toString substResult)
