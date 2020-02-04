namespace FunctionalApproach.Chapter5

type GenTree<'a> = GenNode of 'a * GenTree<'a> list

[<RequireQualifiedAccess>]
module GenTree =

    module private TreeParser =
        type Token = Ident of string | LeftParen | RightParen |  Comma

        let tokenize (input: string) : Token list =
            let rec tokenizeInternal (cs: char list) (tokens: Token list) (currentIdentO : string option) : Token list =
                let addTokenAndContinue token rest =
                    match currentIdentO with
                    | None -> tokenizeInternal rest (token::tokens) None
                    | Some(ident) -> tokenizeInternal rest (token::(Ident ident)::tokens) None
                match cs, currentIdentO with
                | ','::rest, _ ->  addTokenAndContinue Comma rest
                | '('::rest, _ -> addTokenAndContinue LeftParen rest
                | ')'::rest, _ -> addTokenAndContinue RightParen rest
                | ' '::rest, _ ->tokenizeInternal rest tokens currentIdentO
                | c::rest, None -> tokenizeInternal rest tokens (Some (c.ToString ()) )
                | c::rest, Some(ident) -> tokenizeInternal rest tokens (Some (ident + c.ToString()))
                | [], None -> tokens
                | [], Some(ident) -> (Ident ident)::tokens
            let chars = Seq.toList input
            tokenizeInternal chars [] None |> List.rev

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

        let rec getChildTokens (tokens: Token list) : (Token list) list =
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
        fun string_to_data expr ->
            let tokens = TreeParser.tokenize expr
            TreeParser.parseNode string_to_data tokens

    let rec size: GenTree<'a> -> int = fun (GenNode (a, l)) -> 1 + List.sumBy size l     
