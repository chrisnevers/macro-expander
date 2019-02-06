open Set
open List
open Macro_ast

module IntSet = Set.Make(struct type t = int let compare = compare end)

type scopes = IntSet.t

type syntax =
  | SyntaxObj of s_exp * scopes

let str_syntax s = match s with
  | SyntaxObj (e, s) ->
    "SyntaxObj(" ^ str_s_exp e ^ ", {" ^
    IntSet.fold (fun i acc -> acc ^ string_of_int i ^ " ") s "" ^ "})"

let syntax_e = function SyntaxObj (e, s) -> e

let syntax_s = function SyntaxObj (e, s) -> s

let rec datum_to_syntax d =
  let _rec d = datum_to_syntax d in
  match d with
  | SSymbol _ -> SyntaxObj (SQuote d, IntSet.empty) :: []
  | SList ds -> flatten (map _rec ds)

let rec syntax_to_datum s =
  let _rec s = syntax_to_datum [s] in
  match s with
  | SyntaxObj (e, s) :: [] -> e
  | ss -> SQuote (SList (map get_datum (map _rec ss)))
