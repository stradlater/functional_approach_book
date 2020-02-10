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