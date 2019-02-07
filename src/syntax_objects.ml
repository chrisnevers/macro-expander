open Set
open List
open Macro_ast
open Scope

module ScopeSet = Set.Make(struct type t = scope let compare = compare end)

type scope_set = ScopeSet.t

type syntax =
  | SyntaxObj of s_exp * scope_set
  | SyntaxList of syntax list

let rec str_syntax s = match s with
  | SyntaxObj (e, s) ->
    "SyntaxObj(" ^ str_s_exp e ^ ", {" ^
    ScopeSet.fold (fun i acc -> acc ^ string_of_int i ^ " ") s "" ^ "})"
  | SyntaxList ss -> "[" ^ String.concat ", " (map str_syntax ss) ^ "]"

let rec syntax_e e = match e with
  | SyntaxObj (e, s) -> e
  | SyntaxList s -> macro_error "syntax_e: expected syntax object"

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

let add_scope = ScopeSet.add

let flip_scope sc scs =
  let open ScopeSet in
  if mem sc scs then remove sc scs else add sc scs
