open Set
open List
open Macro_ast
open Scope

let rec str_syntax s = match s with
  | SyntaxObj (e, s) ->
    "SyntaxObj(" ^ str_s_exp e ^ ", {" ^
    ScopeSet.fold (fun i acc -> acc ^ string_of_int i ^ " ") s "" ^ "})"
  | SyntaxList ss -> "[" ^ String.concat ", " (map str_syntax ss) ^ "]"

let syntax e scopes = SyntaxObj (e, ScopeSet.of_list scopes)

let mk_syntax e = SyntaxObj (e, ScopeSet.empty)

let rec syntax_e e = match e with
  | SyntaxObj (e, s) -> e
  | SyntaxList s ->
    match s with
    | SyntaxObj (SId "lambda", sc1)
      :: SyntaxList [SyntaxObj (SId arg, sc2)]
      :: SyntaxObj (body, sc3):: [] -> SLambda (arg, body)
    | SyntaxObj (SId "let-syntax", sc1)
      :: SyntaxObj (SId id, sc2)
      :: SyntaxObj (ie, sc3) :: SyntaxObj (be, sc4) :: [] ->
        SLetSyntax (id, ie, be)
    | SyntaxObj (SId "quote", sc1)
      :: SyntaxObj (SQuote s, sc2) :: [] -> SQuote s
    | SyntaxObj (SId "quote-syntax", sc1)
      :: SyntaxObj (SQuote s, sc2) :: []-> SQuoteSyntax s
    | SyntaxObj (id, sc) :: SyntaxList t :: [] ->
      SApply (id, map syntax_e t)

let rec syntax_s e = match e with
  | SyntaxObj (e, s) -> s
  | SyntaxList s -> macro_error "syntax_s: expected syntax object"

let rec datum_to_syntax d =
  let _rec d = datum_to_syntax d in
  match d with
  | SSymbol _ -> SyntaxObj (SQuote d, ScopeSet.empty)
  | SList ds -> SyntaxList (map _rec ds)

let rec syntax_to_datum s =
  let _rec s = syntax_to_datum s in
  match s with
  | SyntaxObj (e, s) -> e
  | SyntaxList ss -> SQuote (SList (map get_datum (map _rec ss)))

let rec adjust_scope so sc op =
  match so with
  | SyntaxObj (e, s) -> SyntaxObj (e, op sc s)
  | SyntaxList ss -> SyntaxList (map (fun s -> adjust_scope s sc op) ss)

let add_scope so sc = adjust_scope so sc ScopeSet.add

let flip_scope_op sc scs =
  let open ScopeSet in
  if mem sc scs then remove sc scs else add sc scs

let flip_scope so sc = adjust_scope so sc flip_scope_op
