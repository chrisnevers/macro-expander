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

and s_datum =
  | SSymbol of string
  | SList of s_datum list
  | SSO of syntax

and syntax =
  | SyntaxObj of s_exp * scope_set
  | SyntaxList of syntax list

let rec str_s_exp e = match e with
  | SId id -> id
  | SQuote d -> "'" ^ str_s_datum d
  | SQuoteSyntax d -> "(QUOTE-SYNTAX " ^ str_s_datum d ^ ")"
  | SLambda (id, e) -> "(LAMBDA (" ^ id ^ ") " ^ str_s_exp e ^ ")"
  | SApply (id, args) ->
    "(" ^ str_s_exp id ^ " " ^ String.concat " " (map str_s_exp args) ^ ")"
  | SLetSyntax (id, p, b) ->
    "(LET-SYNTAX ([" ^ id ^ " " ^ str_s_exp p ^ "]) " ^ str_s_exp b ^ ")"

and str_s_datum a = match a with
  | SSymbol s -> s
  | SList s -> "(" ^ String.concat " " (map str_s_datum s) ^ ")"

let get_datum s = match s with
  | SQuote d | SQuoteSyntax d -> d
  | _ -> macro_error "get_datum: expected expr with datum"
