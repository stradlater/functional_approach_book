namespace Chapter5

type Term<'a, 'b> = 
    | Term of 'a * Term<'a, 'b> list
    | Var of 'b

module Term =

    module private TermParser =

        let rec parseTerms tokens level : Term<string, string> list =
            match tokens, level with
            | Token.Ident(ident)::Token.LeftParen::rest, 0 ->
                let children = parseTerms rest 0
                Term( ident, children)::(parseTerms  (Token.LeftParen::rest) 0)
            | Token.Ident(ident)::rest, 0 -> Var(ident)::(parseTerms rest 0)
            | Token.Ident(_)::rest, _ -> parseTerms rest level
            | Token.LeftParen::rest, _ -> parseTerms rest (level + 1)
            | Token.RightParen::_, 0 -> []
            | Token.RightParen::rest, _ -> parseTerms rest (level - 1)
            | [], _ -> []

        let parseTerm (tokens: Token.Token list) =
            List.head (parseTerms tokens 0)

    let ofString : string -> Term<string,string> = Token.tokenize >> TermParser.parseTerm

    let rec toString : Term<string,string> -> string =
        function
        | Term(d, []) -> d + "()"
        | Term(d, t::ts) -> d + "(" + (List.fold (fun str te -> str + "," + (toString te)) (toString t) ts) + ")"
        | Var d -> d

    let rec trav f g start v =
        function
        | Term(d, ts) -> f(d, List.foldBack (g << (trav f g start v)) ts start)
        | Var d -> (v d)

    /// Computes the set ov variables appearing in a term
    let vars t = trav snd Set.union Set.empty Set.singleton t

    /// Tests if the given variable occurs in the given term
    let occurs v t = 
        let vs = vars t
        Set.contains v vs


type Substitution<'a, 'b> when 'b : comparison = Map<'b, Term<'a, 'b>>

module Substitution =

    let toString (subst : Substitution<string, string>) =
        subst 
        |> Map.toList
        |> List.map (fun (a, term) -> a + "  -->  " + (Term.toString term))
        |> List.reduce (fun a b -> a + System.Environment.NewLine + b)

    /// Apply a substitution to a term
    let rec apply subst =
        function
        | Term(f, tl) -> Term(f, List.map (apply subst) tl)
        | Var(x) as v -> Map.tryFind x subst |> Option.defaultValue v
        
    let compose subst1 subst2 =
        let subst1AppliedToSubst2 = (Map.map (fun _ t -> (apply subst1 t)) subst2)
        let subst1NotInSubst2 = Map.filter (fun k _ -> not (Map.containsKey k subst2) ) subst1
        Map( Seq.concat [Map.toSeq subst1AppliedToSubst2; Map.toSeq subst1NotInSubst2 ])
        
        


