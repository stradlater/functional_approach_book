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
        //let subst1NotInSubst2 = Map.filter (fun k _ -> not (Map.containsKey k subst2) ) subst1
        Map.fold (fun s k t -> Map.add k t s) subst1 subst1AppliedToSubst2

    let addSubst (substs : Substitution<'a, 'b>) newSubsts =
        List.fold (fun s (v, t) -> 
            if Map.containsKey v s then
                let t0 = Map.find v s
                if t0 <> t then
                    failwithf "Different substitution of same var already added"
                else
                    s                
            else
                Map.add v t s
        ) substs newSubsts

    /// Produce a substitution that matches t1 with t2
    let matchTerms term1 term2 =
        let rec matchRec t1 t2 subst =
            match t1, t2 with
            | Var(v), t -> addSubst subst [(v, t)]
            | t, Var(_) -> failwithf "No match - term-var"
            | Term(id1, children1), Term(id2, children2) ->
                if id1 = id2 && (List.length children1) = (List.length children2) then
                    List.fold (fun s (t1, t2) -> matchRec t1 t2 s) subst (List.zip children1 children2)
                else
                    failwithf "No match - term error"
        matchRec term1 term2 Map.empty

    let rec unify t1 t2 =
        match t1, t2 with    
        | (Var v, t) -> 
            if (Var v) = t then 
                Map.empty
            else if Term.occurs v t then
                failwithf "var occurs in term"
            else
                Map.ofList [(v, t)]
        | (t, Var v) -> 
            if Term.occurs v t then
                failwithf "var occurs in term"
            else
                Map.ofList [(v, t)]
        | (Term(op1, children1), Term(op2, children2)) ->
            if op1 = op2 then
                let substUnif s (term1, term2) =
                    compose (unify (apply s term1) (apply s term2)) s
                List.fold substUnif Map.empty (List.zip children1 children2)                
            else
                failwithf "different ops"



        
        


