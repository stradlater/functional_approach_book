namespace Chapter5

type GenTree<'a> = GenNode of 'a * GenTree<'a> list

[<RequireQualifiedAccess>]
module GenTree =

    module private TreeParser =
        type Token = Ident of string | LeftParen | RightParen |  Comma
        type TState = State of Token list * string

        let tokenize (input: string) : Token list =
            let readChar (State (ts, ident)) (c: char) : TState =
                let tokensWithAddedIdent ident ts = if ident = "" then ts else Ident(ident)::ts
                match c with
                | ',' -> State(Comma::(tokensWithAddedIdent ident ts), "")
                | '(' -> State(LeftParen::(tokensWithAddedIdent ident ts), "")
                | ')' -> State(RightParen::(tokensWithAddedIdent ident ts), "")
                | ' ' -> State(ts, ident)
                | c -> State(ts, ident + c.ToString())
            let chars = Seq.toList input
            let (State(tokens, _)) = List.fold readChar (State ([], "")) chars
            tokens |> List.rev

        let rec splitOnChildren tokens parenLevel (currentChild: Token list) (childArr: (Token list) list) =
            match tokens with
            | Token.LeftParen::rest -> 
                if parenLevel = 0 then
                    splitOnChildren rest (parenLevel + 1) currentChild childArr
                else
                    splitOnChildren rest (parenLevel + 1) (Token.LeftParen::currentChild) childArr
            | Token.RightParen::rest -> 
                if parenLevel = 1 then
                    currentChild::childArr // Last child
                else
                    splitOnChildren rest (parenLevel - 1) (Token.RightParen::currentChild) childArr
            | Token.Comma::rest -> 
                if parenLevel = 1 then
                    splitOnChildren rest parenLevel [] (currentChild::childArr)
                else
                    splitOnChildren rest parenLevel (Token.Comma::currentChild) childArr
            | token::rest -> splitOnChildren rest parenLevel (token::currentChild) childArr
            | [] -> childArr

        let getChildTokens (tokens: Token list) : (Token list) list =
            let childrenSplit = splitOnChildren tokens 0 [] []
            childrenSplit |> List.map List.rev |> List.rev

        let rec parseChildren (string_to_data : string -> 'a) (tokens: Token list) : GenTree<'a> list =
            let childTokens = getChildTokens tokens
            childTokens |> List.map (parseNode string_to_data)

        and parseNode (string_to_data : string -> 'a) (tokens: Token list) =
            match tokens with
            | Ident(ident)::rest -> GenNode ((string_to_data ident), (parseChildren string_to_data rest))
            | _ -> failwith "Unexpected token array"


    let ofString : (string -> 'a) -> string -> GenTree<'a> =
        fun string_to_data -> TreeParser.tokenize >> TreeParser.parseNode string_to_data

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