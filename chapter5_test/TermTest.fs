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


[<Fact>]
let ``Test Compose substitutions`` () =
    let subst1 = Map.ofList [("x", Term.ofString "g(x,y)")]
    let subst2 = Map.ofList [("y", Term.ofString "h(x,z)")]
    let composed = Substitution.compose subst1 subst2
    let strComp = Substitution.toString composed

    Assert.Equal("x  -->  g(x,y)\r\ny  -->  h(g(x,y),z)", strComp)