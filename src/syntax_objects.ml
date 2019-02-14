open Set
open List
open Macro_ast
open Scope

let syntax e scopes = SyntaxObj (e, ScopeSet.of_list scopes)

let mk_syntax e = SyntaxObj (e, ScopeSet.empty)

let rec syntax_s e = match e with
  | SyntaxObj (e, s) -> s
  | SyntaxList s -> macro_error "syntax_s: expected syntax object"

let rec sexp_to_syntax s sc =
  let _rec s = sexp_to_syntax s sc in
  match s with
  | SLambda (id, body) ->
    SyntaxList [SyntaxObj(SId "lambda", sc); SyntaxList [SyntaxObj (SId id, sc)]; _rec body]
  | SId id -> SyntaxObj(s, sc)
  | SApply (fn, args) -> SyntaxList [_rec fn; SyntaxList (map _rec args)]
  | SQuote d -> SyntaxList [SyntaxObj (SId "quote", sc); datum_to_syntax d sc]
  | SLetSyntax (l, r, body) ->
    SyntaxList [SyntaxObj (SId "let-syntax", sc); _rec (SId l); _rec r; _rec body]
  | SQuoteSyntax d -> SyntaxList [SyntaxObj (SId "quote-syntax", sc); datum_to_syntax d sc]
  | SQuoteSO so -> SyntaxList [SyntaxObj (SId "quote-syntax", sc); so]

and datum_to_syntax d sc =
  let _rec d = datum_to_syntax d sc in
  match d with
  | SSymbol _ -> SyntaxObj (SQuote d, sc) (* ScopeSet.empty *)
  | SList ds -> SyntaxList (map _rec ds)

let datum_to_syntax_empty d = datum_to_syntax d ScopeSet.empty
let sexp_to_syntax_empty s = sexp_to_syntax s ScopeSet.empty

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
