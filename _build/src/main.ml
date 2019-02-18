exception MacroError of string
let macro_error msg = raise (MacroError msg)

(* Gensym: creates unique names/integers *)

module Gensym =
  struct
    let cnt = ref 0

    let gen_int () =
      let c = !cnt in
      cnt := !cnt + 1;
      c

    let gen_str str =
      let c = !cnt in
      cnt := !cnt + 1;
      str ^ (string_of_int c)

    let reset () = cnt := 0
end


(* Scope: scope of expressions will be a unique int *)

type scope = int

module ScopeSet = Set.Make(struct type t = scope let compare = compare end)

type scope_set = ScopeSet.t

let scope () = Gensym.gen_int ()


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

type datum =
  | DSym of string
  | DList of datum list

type exp =
  | SId           of string
  | SApp          of exp list
  | SLambda       of exp * exp
  | SLetStx       of exp * exp * exp
  | SQuote        of datum
  | SQuoteStx     of datum
  | SQuoteStxObj  of syntax

and syntax =
  | SO of exp * scope_set
  | SOList of syntax list

let rec str_datum d =
  match d with
  | DSym s -> s
  | DList ds -> "(" ^ String.concat " " (List.map str_datum ds) ^ ")"

let str_scope sc =
  let l = ScopeSet.elements sc in
  "{" ^ String.concat " " (List.map string_of_int l)  ^ "}"

let rec str_exp e =
  match e with
  | SId id -> id
  | SApp es -> "(" ^ String.concat " " (List.map str_exp es) ^ ")"
  | SLambda (arg, e) -> "(LAMBDA (" ^ str_exp arg ^ ") " ^ str_exp e ^ ")"
  | SLetStx (id, rhs, e) -> "(LET-SYNTAX [" ^ str_exp id ^ " " ^ str_exp rhs
                            ^ "] " ^ str_exp e ^ ")"
  | SQuote d -> "(QUOTE " ^ str_datum d ^ ")"
  | SQuoteStx d -> "(QUOTE-STX " ^ str_datum d ^ ")"
  | SQuoteStxObj s -> "(QUOTE-STX-OBJ " ^ str_syntax s ^ ")"

and str_syntax s =
  match s with
  | SO (e, sc) -> "(StxObj " ^ str_exp e ^ " " ^ str_scope sc ^ ")"
  | SOList ss -> "(StxList [" ^ String.concat " " (List.map str_syntax ss) ^ "])"

let get_datum s =
  match s with
  | SQuote d | SQuoteStx d -> d
  | _ -> macro_error ("Get_datum: Expected SQuote or SQuoteStx, but received: "
          ^ str_exp s)


let stx_list e sc = SO (e, ScopeSet.of_list sc)

let stx_set e sc = SO (e, sc)

let stx_mt e = SO (e, ScopeSet.empty)

let rec syntax_s s =
  match s with
  | SO (e, sc) -> sc
  | _ -> macro_error ("Syntax_s: Expected SO, but received: " ^ str_syntax s)

let rec datum_to_stx ?(sc=ScopeSet.empty) d =
  let _rec d = datum_to_stx d ~sc:sc in
  match d with
  | DSym id -> SO (SId id, sc)
  | DList ds -> SOList (List.map _rec ds)

let rec exp_to_stx ?(sc=ScopeSet.empty) s =
  let _rec e = exp_to_stx e ~sc:sc in
  match s with
  | SId id -> SO (s, sc)
  | SApp es -> SOList (List.map _rec es)
  | SLambda (arg, e) ->
    SOList [_rec (SId "lambda"); _rec arg; _rec e]
  | SLetStx (id, rhs, e) ->
    SOList [_rec (SId "let-syntax"); _rec id; _rec rhs; _rec e]
  | SQuote d -> SOList [_rec (SId "quote"); datum_to_stx d]
  | SQuoteStx d -> SOList [_rec (SId "quote-syntax"); datum_to_stx d]
  | SQuoteStxObj s -> SOList [_rec (SId "quote-syntax-obj"); s]

let has_id s exp =
  match s with
  | SO (SId id, _) when id = exp -> true
  | _ -> false

let is_id s = match s with
  | SO (SId _, _) -> true
  | _ -> false

let is_quote s = has_id s "quote"
let is_quotestx s = has_id s "quote-syntax"
let is_quotestxobj s = has_id s "quote-syntax-obj"
let is_lambda s = has_id s "lambda"
let is_letstx s = has_id s "let-syntax"

let rec stx_to_datum s =
  match s with
  | SO (SId id, _) -> DSym id
  | SOList ss -> DList (List.map stx_to_datum ss)

let rec stx_to_exp s =
  let _rec e = stx_to_exp e in
  match s with
  | SO (e, _) -> e
  | SOList es -> match es with
    | s :: t when is_quote s -> SQuote (stx_to_datum (List.hd t))
    | s :: t when is_quotestx s -> SQuoteStx (stx_to_datum (List.hd t))
    | s :: t when is_quotestxobj s -> SQuoteStxObj (List.hd t)
    | s :: arg :: body :: [] when is_lambda s -> SLambda (_rec arg, _rec body)
    | s :: l :: rhs :: body :: [] when is_letstx s ->
      SLetStx (_rec l, _rec rhs, _rec body)
    | s :: t -> SApp (_rec s :: List.map _rec t)

let syntax_e = stx_to_exp

let rec adjust_scope stxobj scope op =
  let _rec s = adjust_scope s scope op in
  match stxobj with
  | SO (e, s) -> SO (e, op scope s)
  | SOList ss -> SOList (List.map _rec ss)

let flip scope scopes =
  let open ScopeSet in
  if mem scope scopes then remove scope scopes else add scope scopes

let flip_scope stxobj scope = adjust_scope stxobj scope flip

let add_scope stxobj scope = adjust_scope stxobj scope ScopeSet.add

type binding = (syntax, int) Hashtbl.t

let all_bindings : binding = Hashtbl.create 10

let add_binding id binding = Hashtbl.add all_bindings id binding

let find_all_matching_bindings id =
  let open Hashtbl in
  fold (fun k v acc ->
    let same_id = syntax_e id = syntax_e k in
    let is_subset = ScopeSet.subset (syntax_s k) (syntax_s id) in
    acc @ if same_id && is_subset then [k] else []
  ) all_bindings []

let rec get_largest_scope so cur acc =
  let open ScopeSet in
  match so with
  | [] -> acc
  | SOList s :: t ->
    macro_error ("Get_largest_scope: Did not expected SOList: " ^ str_syntax (SOList s))
  | SO (e, sc) :: t ->
    match List.length (elements sc) with
    | len when len > cur -> get_largest_scope t len [SO (e, sc)]
    | len when len = cur -> get_largest_scope t cur (SO (e, sc) :: acc)
    | _ -> get_largest_scope t cur acc

let rec check_unambigious max_id candidate_ids =
  let open ScopeSet in
  match candidate_ids with
  | h :: t ->
    if subset (syntax_s h) (syntax_s max_id) then
      check_unambigious max_id t
    else macro_error ("Check_ambigious: ambigious scope: " ^ str_syntax max_id)
  | [] -> ()

let resolve id =
  let open List in
  let candidate_ids = find_all_matching_bindings id in
  match candidate_ids with
  | [] -> (- 1)
  | _ ->
    (* compare all candidate objects' scope length & choose greatest *)
    let max_id = hd (get_largest_scope candidate_ids 0 []) in
    check_unambigious max_id candidate_ids;
    Hashtbl.find all_bindings max_id

let core_scope = scope ()

let _id id = SId id

let core_form_ids = ["lambda"; "let-syntax"; "quote"; "quote-syntax"]

let core_forms = List.map _id core_form_ids

let core_primitive_ids = ["datum->syntax"; "syntax->datum"; "syntax-e"; "list";
  "cons"; "first"; "second"; "rest"; "map"]

let core_primitives = List.map _id core_primitive_ids

let add_core () =
  let open List in
  let sym = map _id (core_primitive_ids @ core_form_ids) in
  iter (fun s -> add_binding (stx_list s [core_scope]) core_scope) sym

let _ = add_core ()

let introduce stx = add_scope stx core_scope

type macro =
  | Var
  | MacroFn of (syntax -> syntax)

let env_extend env key value = Hashtbl.add env key value

let env_lookup env binding = try
  Hashtbl.find env binding
  with Not_found -> macro_error ("Env_lookup: Could not find binding: " ^ string_of_int binding)

let apply_transformer fn stx =
  let intro_scope = scope () in
  let intro_s = add_scope stx intro_scope in
  let transformed_s = fn intro_s in
  flip_scope transformed_s intro_scope

let expand_id s env =
  let open ScopeSet in
  let binding = resolve s in
  if binding = (- 1) then
    macro_error ("Expand_id: Free variable detected: " ^ str_syntax s)
  else
    let e = syntax_e s in
    if List.mem e core_primitives then s
    else if List.mem e core_forms then
      macro_error ("Expand_id: Bad syntax. Use of core form: " ^ str_syntax s)
    else
      match env_lookup env binding with
      | Var -> s
      | _ -> macro_error ("Expand_id: Bad syntax: " ^ str_syntax s)

let rec expand ?(env=Hashtbl.create 10) stx =
  match stx with
  | SO (e, _) -> expand_id stx env
  | SOList es -> match es with
    | s :: t when is_quote s || is_quotestx s || is_quotestxobj s -> stx
    | s :: arg :: body :: [] when is_lambda s -> expand_lambda s arg body env
    | s :: l :: rhs :: body :: [] when is_letstx s ->
      expand_let_stx s l rhs body env
    | s :: t when is_id s -> expand_id_app stx env
    | s :: t -> expand_app stx env

and expand_lambda lam arg body env =
  let sc = scope () in
  let id = add_scope arg sc in
  let binding = scope () in
  add_binding id binding;
  env_extend env binding Var;
  let e = expand (add_scope body sc) ~env:env in
  SOList [lam; id; e]

and expand_let_stx let_id lhs rhs body env =
  let sc = scope () in
  let id = add_scope lhs sc in
  let binding = scope () in
  add_binding id binding;
  let value = eval_for_syntax_binding rhs in
  env_extend env binding value;
  expand (add_scope body sc) ~env:env

and expand_id_app stx env =
  let SOList (id::_) = stx in
  let binding = resolve id in
  let value = if binding = 0 then Var else env_lookup env binding in
  match value with
  | MacroFn fn -> expand (apply_transformer fn stx) ~env:env
  | _ -> expand_app stx env

and expand_app stx env =
  let _expand s = expand s ~env:env in
  match stx with
  | SO _ -> _expand stx
  | SOList t -> SOList (List.map _expand t)

and eval_for_syntax_binding stx =
  let so = expand stx in
  eval_compiled (compile so)

(* TBD *)
and convert t =
  match t with
  | SOList [SO (SId "lambda", _); SOList [arg]; body] ->
    exp_to_stx (SLambda (stx_to_exp arg, stx_to_exp body))
  | s -> s
  | _ -> macro_error ("Converted to invalid syntax: " ^ str_syntax t)

and eval e stx tbl =
  (* print_endline ("eval : " ^ str_exp e); *)
  match e with
  | SId _ | SQuote _ | SQuoteStx _ -> exp_to_stx e
  | SLambda (arg, body) -> exp_to_stx e
  | SQuoteStxObj s -> s
  | SApp (SId "list"::t) ->
    convert (SOList (List.map (fun v -> eval v stx tbl) t))
  | SApp (SId "second"::t::[]) ->
    let e = stx_to_exp (eval t stx tbl) in
    let e_stx = Hashtbl.find tbl e in
    let SOList (_::snd::_) = e_stx in
    snd
  | SApp (SId "first"::t::[]) ->
    let e = stx_to_exp (eval t stx tbl) in
    let e_stx = Hashtbl.find tbl e in
    let SOList (fst::_) = e_stx in
    fst
  | _ -> SO (SId "list", ScopeSet.of_list [0])

and eval_compiled exp =
  MacroFn (fun stx ->
    match exp with
    | SLambda (arg, body) ->
      let binding = Hashtbl.create 10 in
      Hashtbl.add binding arg stx;
    (* introduce *) eval body stx binding
  )

(* TBD *)
and compile stx =
  let open List in
  let _rec s = compile s in
  match stx with
  | SO (SId id, _) when mem id core_primitive_ids || mem id core_form_ids -> SId id
  | SO (SId id, _) ->
    let binding = resolve stx in
    SId (id ^ "_" ^ string_of_int binding)
  | SOList es -> match es with
    | s :: t when is_quote s -> SQuote (stx_to_datum (List.hd t))
    | s :: t when is_quotestx s -> SQuoteStxObj (List.hd t)
    | s :: t when is_quotestxobj s -> SQuoteStxObj (List.hd t)
    | s :: arg :: body :: [] when is_lambda s -> SLambda (_rec arg, _rec body)
    | s :: l :: rhs :: body :: [] when is_letstx s ->
      SLetStx (_rec l, _rec rhs, _rec body)
    | s :: t -> SApp (_rec s :: List.map _rec t)


let () = print_endline "Hello world!"
