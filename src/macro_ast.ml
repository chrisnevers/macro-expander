open Scope
open List

exception MacroError of string
let macro_error msg = raise (MacroError msg)

(*
  expr :=
    | (lambda (<id>) <expr>)      function
    | <id>                        var
    | (<expr> <expr> ...)         function call
    | (quote <datum>)             literal data
    | (let-syntax ([<id> <expr>]) macro binding
        <expr>)
    | (quote-syntax <datum>)      literal syntax
 *)

module ScopeSet = Set.Make(struct type t = scope let compare = compare end)

type scope_set = ScopeSet.t

type s_exp =
  | SLambda of string * s_exp
  | SId of string
  | SApply of s_exp * s_exp list
  | SQuote of s_datum
  | SLetSyntax of string * s_exp * s_exp
  | SQuoteSyntax of s_datum
  | SQuoteSO of syntax (* used for preserving scopes during compile time eval *)

and s_datum =
  | SSymbol of string
  | SList of s_datum list

and syntax =
  | SyntaxObj of s_exp * scope_set
  | SyntaxList of syntax list

let rec str_s_exp e = match e with
  | SId id -> id
  | SQuote d -> "'" ^ str_s_datum d
  | SQuoteSO d -> "(QUOTE-SO " ^ str_syntax d ^ ")"
  | SQuoteSyntax d -> "(QUOTE-SYNTAX " ^ str_s_datum d ^ ")"
  | SLambda (id, e) -> "(LAMBDA (" ^ id ^ ") " ^ str_s_exp e ^ ")"
  | SApply (id, args) ->
    "(" ^ str_s_exp id ^ " " ^ String.concat " " (map str_s_exp args) ^ ")"
  | SLetSyntax (id, p, b) ->
    "(LET-SYNTAX ([" ^ id ^ " " ^ str_s_exp p ^ "]) " ^ str_s_exp b ^ ")"

and str_s_datum a = match a with
  | SSymbol s -> s
  | SList s -> "(" ^ String.concat " " (map str_s_datum s) ^ ")"

and str_syntax s = match s with
  | SyntaxObj (e, s) ->
    "SyntaxObj(" ^ str_s_exp e ^ ", {" ^
    ScopeSet.fold (fun i acc -> acc ^ string_of_int i ^ " ") s "" ^ "})"
  | SyntaxList ss -> "[" ^ String.concat ", " (map str_syntax ss) ^ "]"

let get_datum s = match s with
  | SQuote d | SQuoteSyntax d -> d
  | _ -> macro_error "get_datum: expected expr with datum"

let rec syntax_e e = match e with
  | SyntaxObj (e, s) -> e
  | SyntaxList s ->
    match s with
    | SyntaxObj (SId "lambda", sc1)
      :: SyntaxList [SyntaxObj (SId arg, sc2)]
      :: body :: [] -> SLambda (arg, syntax_e body)
    | SyntaxObj (SId "let-syntax", sc1)
      :: SyntaxObj (SId id, sc2)
      :: rhs
      :: body :: [] ->
        SLetSyntax (id, syntax_e rhs, syntax_e body)
    | SyntaxObj (SId "quote", sc1)
      :: SyntaxObj (SQuote s, sc2) :: [] -> SQuote s
    | SyntaxObj (SQuote (SSymbol "quote"), sc1)
      :: SyntaxObj (SQuote s, sc2) :: [] -> SQuote s
    | SyntaxObj (SId "quote-syntax", sc1)
      :: SyntaxObj (SQuote s, sc2) :: []-> SQuoteSyntax s
    | SyntaxObj (id, sc) :: SyntaxList t :: [] ->
      SApply (id, map syntax_e t)
    | _ -> macro_error ("syntax_e: " ^ str_syntax e)

let rec dump_s_exp e = match e with
  | SId id -> id
  | SQuote d -> "'" ^ dump_s_datum d
  | SQuoteSO d -> dump_s_exp (syntax_e d)
  | SQuoteSyntax d -> "(quote " ^ dump_s_datum d ^ ")"
  | SLambda (id, e) -> "(lambda (" ^ id ^ ") " ^ dump_s_exp e ^ ")"
  | SApply (id, args) ->
    "(" ^ dump_s_exp id ^ " " ^ String.concat " " (map dump_s_exp args) ^ ")"
  | SLetSyntax (id, p, b) ->
    "(let-syntax ([" ^ id ^ " " ^ dump_s_exp p ^ "]) " ^ dump_s_exp b ^ ")"

and dump_s_datum a = match a with
  | SSymbol s -> s
  | SList s -> "(" ^ String.concat " " (map dump_s_datum s) ^ ")"
