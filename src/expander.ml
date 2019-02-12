open Macro_ast
open Syntax_objects
open Binding
open Scope
open Gensym

exception ExpandError of string
let expand_error msg = raise (ExpandError msg)

(* binding : id -> binding *)
(* env : binding -> meaning *)

type macro =
  | NotFound
  | Variable
  | MacroFunction

let env_extend env key value =
  Hashtbl.add env key value

let env_lookup env binding = try
  Hashtbl.find env binding
  with Not_found -> expand_error "could not find binding in expansion env"

let variable = Gensym.gen_str "variable"

let expand_id s env =
  let open SExpSet in
  let binding = resolve s in
  if binding = (- 1) then
    expand_error ("free variable: " ^ str_syntax s)
  else if mem (syntax_e s) core_primitives then
    print_endline "core primitive!"
  else if mem (syntax_e s) core_forms then
    expand_error ("bad syntax (core form): " ^ str_syntax s)
  else
    let v = env_lookup env binding in
    match v with
    | Variable -> print_endline "variable"
    | NotFound -> print_endline "not found"
    | _ -> expand_error ("bad syntax: " ^ str_syntax s)

let expand_lambda s env = ()
let expand_let_syntax s env = ()

(* Implement *)
let t so = syntax (SId "list") [0]

let apply_transformer v s =
  let intro_scope = scope () in
  let intro_s = adjust_scope s intro_scope add_scope in
  let transformed_s = t intro_s in
  adjust_scope transformed_s intro_scope flip_scope

let rec expand_id_app s env =
  let SyntaxList [so; _] = s in
  let binding = resolve so in
  let v = env_lookup env binding in
  match v with
  | MacroFunction -> expand' (apply_transformer v s) env
  | _ -> expand_app s env

and expand_app s env =
  let _expand s = expand' s env in
  match s with
  | SyntaxObj _ -> _expand s
  | SyntaxList t -> List.iter _expand t

and expand' s env =
  match syntax_e s with
  | SId id -> print_endline "expand_id"; expand_id s env
  | SLambda _ -> print_endline "expand lambda"; expand_lambda s env
  | SLetSyntax _ -> print_endline "expand-let-syntax"; expand_let_syntax s env
  | SQuote _ -> print_endline "quote: return s"; ()
  | SQuoteSyntax _ ->  print_endline "quote syntax: return s"; ()
  | SApply (SId _, _) -> print_endline "expand_id_app"; expand_id_app s env
  | SApply _ -> print_endline "expand_app"; expand_app s env

let expand s =
  let empty_env : (int, macro) Hashtbl.t = Hashtbl.create 10 in
  expand' s empty_env
