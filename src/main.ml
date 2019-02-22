exception MacroError of string
let macro_error msg = raise (MacroError msg)

let last l =
  let open List in
  hd @@ rev l

let rm_last l =
  let open List in
  rev @@ tl @@ rev l
    (* List.rev (List.tl (List.rev l)) *)

(* Tokens *)
type s_token =
  | StLParen
  | StRParen
  | StLBracket
  | StRBracket
  | StColon
  | StId of string
  | StNum of string
  | StHash of string
  | StQuote
  | StEOF

let str_s_token t = match t with
  | StLParen -> "("
  | StRParen -> ")"
  | StLBracket -> "["
  | StRBracket -> "]"
  | StColon -> ":"
  | StId id -> "Id " ^ id
  | StNum n -> "Num " ^ n
  | StHash n -> "Bool " ^ n
  | StQuote -> "'"
  | StEOF -> "EOF"

(* Lexer *)
let get_stream src ty =
  let open Stream in
  match ty with
  | `File -> of_channel @@ open_in src
  | `String -> of_string src

let is_digit c =
  let open Char in
  let asci = code c in
  asci >= code('0') && asci <= code('9')

let is_alpha c =
  let open Char in
  let asci = code c in
  (asci >= code('A') && asci <= code('Z')) ||
  (asci >= code('a') && asci <= code('z'))

let is_space c = c = ' ' || c = '\n' || c = '\t'

let is_id c = match c with
  | '?' | '!' | '\'' | '#' | '$' | '-' | '_' | '>' -> true
  | _ when is_alpha c || is_digit c -> true
  | _ -> false

let peek_char stream = Stream.peek stream

let rec skip_line stream =
  let c = Stream.next stream in
  match c with
  | '\n' -> next_char stream
  | _ -> skip_line stream

and next_char stream = try
  let c = Stream.next stream in
  match c with
  | s when is_space c -> next_char stream
  | ';' -> skip_line stream
  | c -> Some c
  with Stream.Failure -> None

let rec scan_id stream acc =
  let c = peek_char stream in
  match c with
  | Some c when is_id c ->
    let _ = next_char stream in
    scan_id stream (acc ^ (Char.escaped c))
  | _ -> StId acc

let rec scan_num stream acc =
  let c = peek_char stream in
  match c with
  | Some c when is_digit c ->
    let _ = next_char stream in
    scan_num stream (acc ^ (Char.escaped c))
  | _ -> match acc with
    | _ -> StNum acc

let rec scan_hash stream acc =
  match next_char stream with
  | Some c when is_id c || c = '\\' ->
    let _ = next_char stream in
    scan_hash stream (acc ^ (Char.escaped c))
  | _ -> match acc with
    | _ -> StHash acc
    (* | Some c -> macro_error ("Scan_bool: Expected #t or #f, but received: "
                          ^ (Char.escaped c)) *)

let scan_token stream =
  match next_char stream with
  | None -> StEOF
  | Some '(' -> StLParen
  | Some ')' -> StRParen
  | Some '[' -> StLBracket
  | Some ']' -> StRBracket
  | Some '\'' -> StQuote
  | Some ':' -> StColon
  | Some '#' -> scan_hash stream "#"
  | Some c when is_digit c -> scan_num stream (Char.escaped c)
  | Some c -> scan_id stream (Char.escaped c)

let rec lex program =
  let token = scan_token program in
  match token with
  | StEOF -> token :: []
  | _ -> token :: lex program


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

type ty =
  | TyInt
  | TyBool
  | TyVoid
  | TyChar
  | TyVar of string
  | TyArray of ty
  | TyVector of ty list
  | TyFix of ty
  | TyForAll of string * ty
  | TyFn of ty list * ty

type datum =
  | DSym of string
  | DList of datum list

type exp =
  | SId           of string
  | SApp          of exp list
  | SLambda       of exp * exp
  | SLet          of exp * exp * exp
  | SLetStx       of exp * exp * exp
  | SQuote        of datum
  | SQuoteStx     of datum
  | SQuoteStxObj  of syntax
  | SDefine       of exp * (exp * ty) list * ty * exp * exp

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

let rec str_ty t =
  match t with
  | TyInt -> "Int"
  | TyBool -> "Bool"
  | TyVoid -> "Void"
  | TyChar -> "Char"
  | TyVar s -> s
  | TyArray ty -> "(Array " ^ str_ty ty ^ ")"
  | TyVector tys -> "(Vector " ^ String.concat " " (List.map str_ty tys) ^ ")"
  | TyFix ty -> "(Fix " ^ str_ty ty ^ ")"
  | TyForAll (v, ty) -> "(Forall " ^ v ^ " " ^ str_ty ty ^ ")"
  | TyFn (args, ret) -> "(-> " ^ String.concat " " (List.map str_ty args) ^ " "
                        ^ str_ty ret ^ ")"


let rec str_exp e =
  match e with
  | SId id -> id
  | SApp es -> "(" ^ String.concat " " (List.map str_exp es) ^ ")"
  | SLambda (arg, e) -> "(lambda (" ^ str_exp arg ^ ")\n\t" ^ str_exp e ^ ")"
  | SLet (id, rhs, e) -> "(let ([" ^ str_exp id ^ " " ^ str_exp rhs
                            ^ "])\n" ^ str_exp e ^ ")"
  | SLetStx (id, rhs, e) -> "(let-syntax ([" ^ str_exp id ^ " " ^ str_exp rhs
                            ^ "]) " ^ str_exp e ^ ")"
  | SQuote d -> str_datum d
  | SQuoteStx d -> "(quote-syntax " ^ str_datum d ^ ")"
  | SQuoteStxObj s -> "(quote-syntax-obj " ^ str_syntax s ^ ")"
  | SDefine (id, args, ty, body, nxt) -> "(define (" ^ str_exp id ^ " " ^ str_args args
                        ^ ") : " ^ str_ty ty ^ "\n\t" ^ str_exp body ^ ")\n\n" ^ str_exp nxt

and str_syntax s =
  match s with
  | SO (e, sc) -> "(StxObj " ^ str_exp e ^ " " ^ str_scope sc ^ ")"
  | SOList ss -> "(StxList [" ^ String.concat " " (List.map str_syntax ss) ^ "])"

and str_args s =
  match s with
  | (e, ty) :: t -> "[" ^ str_exp e ^ " : " ^ str_ty ty ^ "]" ^ str_args t
  | [] -> ""

(* Parser *)
let next_token tokens = List.hd !tokens

let get_token tokens =
  let token = next_token tokens in
  tokens := List.tl !tokens;
  token

let expect_token tokens expected =
  let actual = get_token tokens in
  if actual != expected then
    macro_error ("Expected " ^ str_s_token expected ^ ", but received: "
                  ^ str_s_token actual)
  else ()

let rec parse_tys input =
  let next = next_token input in
  match next with
  | StRParen -> []
  | _ ->
    let ty = parse_ty input in
    ty :: parse_tys input


and parse_ty input =
  match get_token input with
  | StId "Int" -> TyInt
  | StId "Bool" -> TyBool
  | StId "Void" -> TyVoid
  | StId "Char" -> TyChar
  | StId id -> TyVar id
  | StLParen ->
    let ty = parse_inner_type input in
    let _ = expect_token input StRParen in
    ty
  | ow -> macro_error ("Parse_ty: Expected a type: " ^ str_s_token ow)

and parse_inner_type input =
  let token = get_token input in
  match token with
  | StId "Array" ->
    let atype = parse_ty input in
    TyArray atype
  | StId "Vector" ->
    let types = parse_tys input in
    TyVector types
  | StId "->" ->
    let ret = parse_tys input in
    TyFn (rm_last ret, last ret)
  | StId "Forall" ->
    let id = parse_id input in
    let ty = parse_ty input in
    TyForAll (id, ty)
  | StId "Fix" ->
    let ty = parse_ty input in
    TyFix ty
  | ow -> macro_error ("Parse_inner_type: Expected a type: " ^ str_s_token ow)

and parse_id input =
  match get_token input with
  | StId id -> id
  | _ -> macro_error "expected identifier"

let rec parse_optional_exps input =
  let next = next_token input in
  match next with
  | StRParen ->
    let _ = expect_token input StRParen in
    []
  | _ -> let e = parse_exp input in
    e :: parse_optional_exps input

and parse_exp input =
  let token = get_token input in
  match token with
  | StLParen ->
    parse_inside_paren input
  | StId id -> SId id
  | StNum n -> SQuote (DSym n)
  | StHash n -> SQuote (DSym n)
  | StQuote -> SQuote (parse_datum input)
  | _ -> macro_error ("parse_exp: unexpected exp: " ^ str_s_token token)

and parse_inside_paren input =
  let token = get_token input in
  match token with
  | StRParen -> macro_error "not expecting: ()"
  | StId "let" -> parse_let input
  | StId "let-syntax" -> parse_let_syntax input
  | StId "quote" -> parse_quote input
  | StId "quote-syntax" -> parse_quote_syntax input
  | StId "lambda" -> parse_lambda input
  | StId "define" -> parse_define input
  | StId id ->
    let exps = parse_optional_exps input in
    SApp (SId id :: exps)
  | _ -> macro_error ("parse_inside_paren: did not expect " ^ str_s_token token ^ " in (")

and parse_let input =
  let _ = expect_token input StLParen in
  let _ = expect_token input StLBracket in
  let id = parse_exp input in
  let rhs = parse_exp input in
  let _ = expect_token input StRBracket in
  let _ = expect_token input StRParen in
  let body = parse_exp input in
  let _ = expect_token input StRParen in
  SLet (id, rhs, body)

and parse_let_syntax input =
  let _ = expect_token input StLParen in
  let _ = expect_token input StLBracket in
  let id = parse_exp input in
  let pattern = parse_exp input in
  let _ = expect_token input StRBracket in
  let _ = expect_token input StRParen in
  let body = parse_exp input in
  let _ = expect_token input StRParen in
  SLetStx (id, pattern, body)

and parse_quote input =
  let d = parse_datum input in
  let _ = expect_token input StRParen in
  SQuote d

and parse_quote_syntax input =
  let d = parse_datum input in
  let _ = expect_token input StRParen in
  SQuoteStx d

and parse_lambda input =
  let _ = expect_token input StLParen in
  let id = parse_exp input in
  let _ = expect_token input StRParen in
  let body = parse_exp input in
  let _ = expect_token input StRParen in
  SLambda (id, body)

and parse_define input =
  let _ = expect_token input StLParen in
  let id = parse_exp input in
  let args = parse_args input [] in
  let _ = expect_token input StRParen in
  let _ = expect_token input StColon in
  let ty = parse_ty input in
  let body = parse_exp input in
  let _ = expect_token input StRParen in
  let nxt = parse_exp input in
  SDefine (id, args, ty, body, nxt)

and parse_args input acc =
  let next = next_token input in
  match next with
  | StRParen -> acc
  | StLBracket ->
    let _ = expect_token input StLBracket in
    let e = SId (parse_id input) in
    let _ = expect_token input StColon in
    let ty = parse_ty input in
    let _ = expect_token input StRBracket in
    parse_args input ((e, ty) :: acc)
  | _ -> macro_error "parse_args: expected arg, i.e. [ or )"

and parse_datums input =
  let next = next_token input in
  match next with
  | StRParen -> []
  | _ -> let d = parse_datum input in
    d :: parse_datums input

and parse_datum input =
  let token = get_token input in
  match token with
  | StId id -> DSym id
  | StNum n -> DSym n
  | StLParen ->
    let ds = parse_datums input in
    let _ = expect_token input StRParen in
    DList ds
  | StQuote -> DList [DSym "quote"; parse_datum input]
  | _ -> macro_error ("parse_datum: " ^ str_s_token token)

let rec parse input =
  let e = parse_exp input in
  match next_token input with
  | StEOF -> e
  | _ -> macro_error "Parse: expected EOF"

let get_datum s =
  match s with
  | SQuote d | SQuoteStx d -> d
  | _ -> macro_error ("Get_datum: Expected SQuote or SQuoteStx, but received: "
          ^ str_exp s)


let stx_list e sc = SO (e, ScopeSet.of_list sc)

let stx_set e sc = SO (e, sc)

let stx_mt e = SO (e, ScopeSet.empty)

let core_scope = scope ()

let stx_core e = SO (e, ScopeSet.of_list [core_scope])

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
  let _args e = args_to_stx e ~sc:sc in
  let _ty e = ty_to_stx e ~sc:sc in
  match s with
  | SId id -> SO (s, sc)
  | SApp es -> SOList (List.map _rec es)
  | SLambda (arg, e) ->
    SOList [_rec (SId "lambda"); _rec arg; _rec e]
  | SLet (id, rhs, e) ->
    SOList [_rec (SId "let"); _rec id; _rec rhs; _rec e]
  | SLetStx (id, rhs, e) ->
    SOList [_rec (SId "let-syntax"); _rec id; _rec rhs; _rec e]
  | SQuote d -> SOList [_rec (SId "quote"); datum_to_stx d]
  | SQuoteStx d -> SOList [_rec (SId "quote-syntax"); datum_to_stx d]
  | SQuoteStxObj s -> SOList [_rec (SId "quote-syntax-obj"); s]
  | SDefine (id, args, ty, e, nxt) ->
    SOList [_rec (SId "define"); SOList (_rec id :: args_to_stx args); _ty ty; _rec e; _rec nxt]

and args_to_stx ?(sc=ScopeSet.empty) args =
  let _rec e = args_to_stx e ~sc:sc in
  let _exp e = exp_to_stx e ~sc:sc in
  let _ty e = ty_to_stx e ~sc:sc in
  match args with
  | (e, ty) :: t -> SOList [_exp e; _ty ty] :: _rec t
  | [] -> []

and ty_to_stx ?(sc=ScopeSet.empty) ty =
  let _rec ty = ty_to_stx ty ~sc:sc in
  match ty with
  | TyInt -> stx_core (SId "Int")
  | TyBool -> stx_core (SId "Bool")
  | TyVoid -> stx_core (SId "Void")
  | TyChar -> stx_core (SId "Char")
  | TyVar id -> stx_core (SId id)
  | TyArray ty -> SOList [stx_core (SId "Array"); _rec ty]
  | TyVector tys -> SOList (stx_core (SId "Vector") :: List.map _rec tys)
  | TyFix ty -> SOList [stx_core (SId "Fix"); _rec ty]
  | TyForAll (v, ty) -> SOList [stx_core (SId "Forall"); stx_core (SId v); _rec ty]
  | TyFn (args, ret) -> SOList (stx_core (SId "->") :: List.map _rec args @ _rec ret :: [])
(*
  | TyVector tys -> "(Vector " ^ String.concat " " (List.map str_ty tys) ^ ")"
  | TyFix ty -> "(Fix " ^ str_ty ty ^ ")"
  | TyForAll (v, ty) -> "(Forall " ^ v ^ " " ^ str_ty ty ^ ")"
  | TyFn (args, ret) -> "(-> " ^ String.concat " " (List.map str_ty args) ^ " "
                        ^ str_ty ret ^ ")"
 *)

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
let is_let s = has_id s "let"
let is_define s = has_id s "define"

let rec stx_to_datum s =
  match s with
  | SO (SId id, _) -> DSym id
  | SOList ss -> DList (List.map stx_to_datum ss)

let stx_to_ty s =
  match s with
  | s when has_id s "Int" -> TyInt
  | s when has_id s "Bool" -> TyBool
  | s when has_id s "Void" -> TyVoid
  | s when has_id s "Char" -> TyChar
  | SO (SId id, _)-> TyVar id

let rec stx_to_exp s =
  let _rec e = stx_to_exp e in
  let _args e = stx_to_args e in
  let _ty e = stx_to_ty e in
  match s with
  | SO (e, _) -> e
  | SOList es -> match es with
    | s :: t when is_quote s -> SQuote (stx_to_datum (List.hd t))
    | s :: t when is_quotestx s -> SQuoteStx (stx_to_datum (List.hd t))
    | s :: t when is_quotestxobj s -> SQuoteStxObj (List.hd t)
    | s :: arg :: body :: [] when is_lambda s -> SLambda (_rec arg, _rec body)
    | s :: l :: rhs :: body :: [] when is_let s ->
      SLet (_rec l, _rec rhs, _rec body)
    | s :: l :: rhs :: body :: [] when is_letstx s ->
      SLetStx (_rec l, _rec rhs, _rec body)
    | s :: SOList (id :: args) :: ty :: body :: nxt :: [] when is_define s ->
      SDefine (_rec id, _args args, _ty ty, _rec body, _rec nxt)
    | s :: t -> SApp (_rec s :: List.map _rec t)

and stx_to_args args =
  let _rec e = stx_to_args e in
  let _exp e = stx_to_exp e in
  let _ty e = stx_to_ty e in
  match args with
  | SOList (e::ty::[]) :: tl -> (_exp e, _ty ty) :: _rec tl
  | [] -> []

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

let _id id = SId id

let core_form_ids = ["lambda"; "let-syntax"; "quote"; "quote-syntax"]

let core_forms = List.map _id core_form_ids

let core_primitive_ids = ["datum->syntax"; "syntax->datum"; "syntax-e"; "list";
  "cons"; "first"; "second"; "third"; "fourth"; "rest"; "map";
  "+"; "-"; "*"; "/"; "%"; "let"; "nth"; "define"; "or"; "if"; "eq?";
  "and"; "begin"; "array-set!"; "array-ref"; "print"; "vector";
  "array"; "vector-ref"; "vector-set!"; "define-type"; "void"; "vector-length";
  "read"; "zero?"; "pos?"; "not"; ">" ; ">="; "<"; "<="; "while"; "neg?";
  "unless"]

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
      (* let _ = print_endline ("expand id: " ^ str_syntax s) in *)
      match env_lookup env binding with
      | Var -> s
      | _ -> macro_error ("Expand_id: Bad syntax: " ^ str_syntax s)

let rec expand ?(env=Hashtbl.create 10) stx =
  match stx with
  | SO (e, _) -> expand_id stx env
  | SOList es -> match es with
    | s :: t when is_quote s || is_quotestx s || is_quotestxobj s -> stx
    | s :: arg :: body :: [] when is_lambda s -> expand_lambda s arg body env
    | s :: l :: rhs :: body :: [] when is_let s ->
      expand_let s l rhs body env
    | s :: l :: rhs :: body :: [] when is_letstx s ->
      expand_let_stx s l rhs body env
    | s :: SOList (id :: args) :: ty :: body :: nxt :: [] when is_define s ->
      expand_define s id args ty body nxt env
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

and expand_let let_id lhs rhs body env =
  let sc = scope () in
  let id = add_scope lhs sc in
  let binding = scope () in
  add_binding id binding;
  let nrhs = expand rhs ~env:env in
  env_extend env binding Var;
  let nbody = expand (add_scope body sc) ~env:env in
  SOList [let_id; id; nrhs; nbody]

and expand_let_stx let_id lhs rhs body env =
  let sc = scope () in
  let id = add_scope lhs sc in
  let binding = scope () in
  add_binding id binding;
  let value = eval_for_syntax_binding rhs in
  env_extend env binding value;
  expand (add_scope body sc) ~env:env

and expand_define define_id id args ty body nxt env =
  let sc = scope () in
  let new_args = SOList (add_scope id sc ::
    List.map (fun arg ->
      match arg with
      | SOList (e::ty::[]) -> SOList [add_scope e sc; ty]
    ) args) in
  let SOList all_ids = new_args in
  List.iter (fun e ->
    match e with
    | SO (_, _) as id
    | SOList [id; _] ->
      (* print_endline ("Expand_define: Adding binding for: " ^ str_syntax id); *)
      let binding = scope () in
      add_binding id binding;
      env_extend env binding Var;
  ) all_ids;
  let new_body = expand (add_scope body sc) ~env:env in
  let new_next = expand (add_scope nxt sc) ~env:env in
  SOList [define_id; new_args; ty; new_body; new_next]

and expand_id_app stx env =
  let SOList (id::_) = stx in
  let binding = resolve id in
  (* let _ = print_endline ("expand id app: " ^ str_syntax id ^ " : " ^ string_of_int binding) in *)
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
  | s -> (* print_endline ("convert: " ^ str_syntax s); *) s
  | _ -> macro_error ("Converted to invalid syntax: " ^ str_syntax t)

and eval e tbl =
  (* print_endline ("eval : " ^ str_exp e); *)
  match e with
  | SId id -> Hashtbl.find tbl e
  | SQuote _ | SQuoteStx _ -> exp_to_stx e
  | SLambda (arg, body) -> exp_to_stx e
  | SQuoteStxObj s -> s
  | SLet (id, rhs, body) ->
      let rhs_res = eval rhs tbl in
      (* ASK: will be printing out let, so assign id to itself.
          eval (SId id) => its value in the env
            - (second stx) => will evaluate actual result
            - (+ x_2 x_2) => will not technically evaluate it.
       *)
      Hashtbl.add tbl id (exp_to_stx id);
      let let_id = stx_core (SId "let") in
      SOList [let_id; stx_mt id; rhs_res; eval body tbl]
  | SApp (SId "list"::t) ->
    convert (SOList (List.map (fun v -> eval v tbl) t))
  | SApp (SId "nth"::t::n::[]) -> begin
    try
      let SOList res = eval t tbl in
      let SOList (_::SO (SId pos, _)::[])= eval n tbl in
      List.nth res ((int_of_string pos) - 1)
    with Failure _ -> macro_error ("Eval: index out of bounds: " ^ str_exp e)
    end
  | SApp (SId "first"::t::[]) ->
    let SOList (fst::_) = eval t tbl in fst
  | SApp (SId "second"::t::[]) ->
    let SOList (_::snd::_) = eval t tbl in snd
  | SApp (SId "third"::t::[]) ->
    let SOList (_::_::thrd::_) = eval t tbl in thrd
  | SApp (SId "fourth"::t::[]) ->
    let SOList (_::_::_::frth::_) = eval t tbl in frth
  | SApp (SId "rest"::t::[]) ->
    let SOList (_::tl) = eval t tbl in SOList tl
  | SApp (SId "cons"::l::r::[]) ->
    SOList [eval l tbl; eval r tbl]
  | SApp (SId "map"::fn::t::[]) ->
    let SOList es = eval t tbl in
    let SLambda (arg, body) = stx_to_exp (eval fn tbl) in
    SOList (List.map (fun e ->
      Hashtbl.add tbl arg e;
      eval body tbl
    ) es)
  (* Primtive application, just process args *)
  | SApp (SId id::t) ->
    let id = stx_core (SId id) in
    SOList (id :: List.map (fun e -> eval e tbl) t)
  | _ -> macro_error ("Eval: cannot eval " ^ str_exp e)

and eval_compiled exp =
  MacroFn (fun stx ->
    match exp with
    | SLambda (arg, body) ->
      let binding = Hashtbl.create 10 in
      Hashtbl.add binding arg stx;
    (* introduce *) eval body binding
    | _ -> macro_error ("Eval_compiled: expected lambda, but received: " ^ str_exp exp)
  )

(* TBD *)
and compile stx =
  let open List in
  let _rec s = compile s in
  match stx with
  | SO (SId id, _) when mem id core_primitive_ids || mem id core_form_ids -> SId id
  | SO (SId id, _) ->
    (* print_endline ("resolving: " ^ id); *)
    let binding = resolve stx in
    SId (id ^ "_" ^ string_of_int binding)
  | SOList es -> match es with
    | s :: t when is_quote s -> SQuote (stx_to_datum (List.hd t))
    | s :: t when is_quotestx s -> SQuoteStxObj (List.hd t)
    | s :: t when is_quotestxobj s -> SQuoteStxObj (List.hd t)
    | s :: arg :: body :: [] when is_lambda s -> SLambda (_rec arg, _rec body)
    | s :: l :: rhs :: body :: [] when is_let s ->
      SLet (_rec l, _rec rhs, _rec body)
    | s :: l :: rhs :: body :: [] when is_letstx s ->
      SLetStx (_rec l, _rec rhs, _rec body)
    | s :: SOList (id :: args) :: ty :: body :: nxt :: [] when is_define s ->
      (* print_endline "compile define"; *)
      SDefine (_rec id, compile_args args, stx_to_ty ty, _rec body, _rec nxt)
    | s :: t -> SApp (_rec s :: List.map _rec t)

and compile_args args =
  match args with
  | SOList [e;ty] :: tl -> (compile e, stx_to_ty ty) :: compile_args tl
  | [] -> []

let () =
  if Array.length Sys.argv = 1 then
    print_endline "Usage: ./main.native {filename}"
  else
    let program = Sys.argv.(1) in
    let stream  = get_stream program `File in
    let tokens  = lex stream in
    let ast     = parse (ref tokens) in
    let out     = stx_to_exp @@ expand @@ introduce @@ exp_to_stx ast in
    print_endline (str_exp out)
