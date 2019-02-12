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

(* Should return copy of table? *)
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
    let _ = print_endline "core primitive!" in s
  else if mem (syntax_e s) core_forms then
    expand_error ("bad syntax (core form): " ^ str_syntax s)
  else
    let v = env_lookup env binding in
    match v with
    | Variable -> print_endline "variable found"; s
    | NotFound -> expand_error ("variable out of context: " ^ str_syntax s)
    | _ -> expand_error ("bad syntax: " ^ str_syntax s)

let decompose_lambda s = match s with
  | SyntaxList [
    lambda_id;
    SyntaxList [arg];
    body
  ] -> (lambda_id, arg, body)

let decompose_let_syntax s = match s with
  | SyntaxList [
    let_id;
    SyntaxList [lhs; rhs];
    body
  ] -> (let_id, lhs, rhs, body)

let rec compile s =
  match s with
  | SyntaxObj (SId id, _) -> SId id (* ? *)
  | SyntaxList t -> match t with
    | SyntaxObj (SId "lambda", _) :: t -> compile_lambda s
    | SyntaxObj (SId "quote", _) :: t -> compile_quote t
    | SyntaxObj (SId "quote-syntax", _) :: t -> compile_quote_syntax t
    | s :: args -> SApply (compile s, List.map compile args)

and compile_lambda s =
  let (_, id, body) = decompose_lambda s in
  let SyntaxObj (SId id_str, _) = id in
  SLambda (id_str, compile body)

and compile_quote s =
  let datum = List.hd s in
  syntax_to_datum datum

and compile_quote_syntax s =
  let datum = List.hd s in
  SQuote (SSO datum)


let eval_compiled l = Variable

(* Implement *)
let t so = syntax (SId "list") [0]

let apply_transformer v s =
  let intro_scope = scope () in
  let intro_s = add_scope s intro_scope in
  let transformed_s = t intro_s in
  flip_scope transformed_s intro_scope

let rec expand_id_app s env =
  let SyntaxList [so; _] = s in
  let binding = resolve so in
  let v = env_lookup env binding in
  match v with
  | MacroFunction -> expand' (apply_transformer v s) env
  | _ -> expand_app s env

and expand_lambda s env =
  let (lambda_id, arg, body) = decompose_lambda s in
  let sc = scope () in
  let id = add_scope arg sc in
  let binding = scope () in
  add_binding id binding;
  env_extend env binding Variable;
  let exp_body = expand' (add_scope body sc) env in
  SyntaxList [lambda_id; SyntaxList [id]; exp_body]

and expand_let_syntax s env =
  let (let_id, lhs_id, rhs, body) = decompose_let_syntax s in
  let sc = scope () in
  let id = add_scope lhs_id sc in
  let binding = scope () in
  add_binding id binding;
  let rhs_val = eval_for_syntax_binding rhs in
  env_extend env binding rhs_val;
  expand' (add_scope body sc) env

and expand_app s env =
  let _expand s = expand' s env in
  match s with
  | SyntaxObj _ -> _expand s
  | SyntaxList t -> SyntaxList (List.map _expand t)

and expand' s env =
  match syntax_e s with
  | SId id -> print_endline "expand_id"; expand_id s env
  | SLambda _ -> print_endline "expand lambda"; expand_lambda s env
  | SLetSyntax _ -> print_endline "expand-let-syntax"; expand_let_syntax s env
  | SQuote _ -> print_endline "quote: return s"; s
  | SQuoteSyntax _ ->  print_endline "quote syntax: return s"; s
  | SApply (SId _, _) -> print_endline "expand_id_app"; expand_id_app s env
  | SApply _ -> print_endline "expand_app"; expand_app s env

and expand s =
  let empty_env : (int, macro) Hashtbl.t = Hashtbl.create 10 in
  expand' s empty_env

and eval_for_syntax_binding rhs =
  (* returns : macro *)
  eval_compiled (compile (expand rhs))
