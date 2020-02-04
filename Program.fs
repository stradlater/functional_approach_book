module Test
    open System
    open FunctionalApproach.Chapter5

    [<EntryPoint>]
    let main argv =

        let treeExpr = "asa(bas, c(foo, bar), d)"

        let gentree = GenTree.ofString id treeExpr

        let size = GenTree.size gentree

        assert (size = 6)

        printfn "Done!"
        0 // return an integer exit code
