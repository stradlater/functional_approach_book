namespace Chapter5

type GenTree<'a> = GenNode of 'a * GenTree<'a> list

[<RequireQualifiedAccess>]
module GenTree =

    module private TreeParser =
        type Token = Ident of string | LeftParen | RightParen
        type TState = State of Token list * string

        let tokenize (input: string) : Token list =
            let readChar (State (ts, ident)) (c: char) : TState =
                let tokensWithAddedIdent ident ts = if ident = "" then ts else Ident(ident)::ts
                match c with
                | ',' -> State((tokensWithAddedIdent ident ts), "")
                | '(' -> State(LeftParen::(tokensWithAddedIdent ident ts), "")
                | ')' -> State(RightParen::(tokensWithAddedIdent ident ts), "")
                | ' ' -> State(ts, ident)
                | c -> State(ts, ident + c.ToString())
            let chars = Seq.toList input
            let (State(tokens, ident)) = List.fold readChar (State ([], "")) chars
            if ident <> "" then
                Ident(ident)::(List.rev tokens)
            else
                tokens |> List.rev 

        let rec parseNodes string_to_data tokens level : GenTree<'a> list =
            match tokens, level with
            | Ident(ident)::LeftParen::rest, 0 -> 
                let children = parseNodes string_to_data rest 0
                GenNode(string_to_data ident, children)::(parseNodes string_to_data (LeftParen::rest) 0)
            | Ident(ident)::rest, 0 -> GenNode(string_to_data ident, [])::(parseNodes string_to_data rest 0)
            | Ident(_)::rest, _ -> parseNodes string_to_data rest level
            | LeftParen::rest, _ -> parseNodes string_to_data rest (level + 1)
            | RightParen::_, 0 -> []
            | RightParen::rest, _ -> parseNodes string_to_data rest (level - 1)
            | [], _ -> []

        let parseNode (string_to_data : string -> 'a) (tokens: Token list) =
            List.head (parseNodes string_to_data tokens 0)

    let ofString : (string -> 'a) -> string -> GenTree<'a> =
        fun string_to_data -> TreeParser.tokenize >> TreeParser.parseNode string_to_data

    let rec toString (dataToString:'a -> string) (GenNode(a, nodes): GenTree<'a>): string =
        (dataToString a) +
            match nodes with
            | [] -> ""
            | c::cs -> "(" + (List.fold (fun str child -> str + "," + (toString dataToString child)) (toString dataToString c) cs) + ")"

    /// The size of the tree is the total number of nodes
    let rec size: GenTree<'a> -> int = fun (GenNode (a, l)) -> 1 + List.sumBy size l

    let private maxInt = 
        function 
        | [] -> 0 
        | xs -> List.max xs

    /// The height of a tree is the length of the longest branch
    let rec height: GenTree<'a> -> int = fun (GenNode (a, l)) -> 1 + maxInt (List.map height l)

    let rec set: GenTree<'a> -> Set<'a> = fun (GenNode (a, l)) -> 
        List.fold Set.union (new Set<'a>([a])) (List.map set l)

    /// Returns the list of nodes in a depth-first, prefix, lefmost traversal
    let rec flat: GenTree<'a> -> 'a list =
        fun (GenNode (x, cs)) -> x::(List.fold (fun s t -> s @ (flat t)) [] cs)


    /// get subtree at the given 0-indexed occurrence
    let rec at (GenNode (x, ts) as t) = 
        function
        | [] -> t
        | i::occ -> at (List.item i ts) occ

    let private mapAtInd xs ind f =
        List.mapi (fun i x -> if i = ind then (f x) else x) xs   

    // replace subtree at the given 0-indexed occurrence
    let rec replaceOcc occ newT (GenNode (x, ts)) =
        match occ with
        | [] -> newT
        | i::occ2 -> GenNode (x, mapAtInd ts i (replaceOcc occ2 newT) )

        