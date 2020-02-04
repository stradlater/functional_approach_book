module Test
    open System
    open FunctionalApproach.Chapter5

    [<EntryPoint>]
    let main argv =

        let treeExpr = "asa(bas, c(foo, bar), d, d)"

        let gentree = GenTree.ofString id treeExpr

        let size = GenTree.size gentree

        assert (size = 7)

        let set = GenTree.set gentree

        printfn "Done!"
        0 // return an integer exit code
