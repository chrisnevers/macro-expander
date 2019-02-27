exception MacroError of string
let macro_error msg = raise (MacroError msg)

let last l =
  let open List in
  hd @@ rev l

let rm_last l =
  let open List in
  rev @@ tl @@ rev l

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
  | StHash n -> "Hash " ^ n
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
  | '.' | '?' | '!' | '\'' | '#' | '$' | '-' | '_' | '>' | '=' -> true
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
  match peek_char stream with
  | Some c when is_id c || c = '\\' ->
    let _ = next_char stream in
    scan_hash stream (acc ^ (Char.escaped c))
  | _ -> match acc with
    | _ -> StHash acc

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
  | TySyntax
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
  | SLambda       of (exp * ty) list * ty * exp
  | STyLambda     of ty * exp
  | SLet          of exp * exp * exp
  | SLetStx       of exp * exp * exp
  | SQuote        of datum
  | SQuoteStx     of datum
  | SQuoteStxObj  of syntax
  | SDefine       of exp * (exp * ty) list * ty * exp * exp
  | SDefineTy     of exp * exp list * exp list * exp
  | SStxCase   of (exp * exp) list

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
  | TySyntax -> "Syntax"
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
  | SLambda (args, ty, e) -> "(lambda (" ^ str_args args ^ ") : " ^ str_ty ty ^ "\n\t" ^ str_exp e ^ ")"
  | STyLambda (ty, e) -> "(Lambda " ^ str_ty ty ^ " " ^ str_exp e ^ ")"
  | SLet (id, rhs, e) -> "(let ([" ^ str_exp id ^ " " ^ str_exp rhs
                            ^ "])\n" ^ str_exp e ^ ")"
  | SLetStx (id, rhs, e) -> "(let-syntax ([" ^ str_exp id ^ " " ^ str_exp rhs
                            ^ "]) " ^ str_exp e ^ ")"
  | SQuote d -> str_datum d
  | SQuoteStx d -> "(quote-syntax " ^ str_datum d ^ ")"
  | SQuoteStxObj s -> "(quote-syntax-obj " ^ str_syntax s ^ ")"
  | SDefine (id, args, ty, body, nxt) -> "(define (" ^ str_exp id ^ " " ^ str_args args
                        ^ ") : " ^ str_ty ty ^ "\n\t" ^ str_exp body ^ ")\n\n" ^ str_exp nxt
  | SDefineTy (id, vars, tys, nxt) -> "(define-type " ^ str_exp id ^ " " ^
    String.concat " " (List.map str_exp vars) ^ " " ^
    String.concat " " (List.map str_exp tys) ^ ")\n" ^ str_exp nxt
  | SStxCase es -> "(syntax-case (" ^ str_cases es ^ "))"

and str_syntax s =
  match s with
  | SO (e, sc) -> "(StxObj " ^ str_exp e ^ " " ^ str_scope sc ^ ")"
  | SOList ss -> "(StxList [" ^ String.concat " " (List.map str_syntax ss) ^ "])"

and str_args s =
  match s with
  | (e, ty) :: t -> "[" ^ str_exp e ^ " : " ^ str_ty ty ^ "]" ^ str_args t
  | [] -> ""

and str_cases s =
  match s with
  | (l, r) :: t -> "[" ^ str_exp l ^ " " ^ str_exp r ^ "]" ^ str_cases t
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
  | StId "Syntax" -> TySyntax
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

let rec parse_optional_ids input =
  let next = next_token input in
  match next with
  | StId id ->
    let e = SId (parse_id input) in
    e :: parse_optional_ids input
  | _ -> []

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
  | StId "define-syntax" -> parse_define_syntax input
  | StId "quote" -> parse_quote input
  | StId "quote-syntax" -> parse_quote_syntax input
  | StId "lambda" -> parse_lambda input
  | StId "Lambda" -> parse_ty_lambda input
  | StId "define" -> parse_define input
  | StId "define-type" -> parse_define_type input
  | StId "syntax-case" -> parse_syntax_case input
  | StId id ->
    let exps = parse_optional_exps input in
    SApp (SId id :: exps)
  | StLParen ->
    let fst = parse_inside_paren input in
    let exps = parse_optional_exps input in
    SApp (fst :: exps)
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

(* like let-syntax but define - for use in imported files *)
and parse_define_syntax input =
  let id = parse_exp input in
  let pattern = parse_exp input in
  let _ = expect_token input StRParen in
  let nxt = parse_exp input in
  SLetStx (id, pattern, nxt)

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
  let ids = parse_args input [] in
  let _ = expect_token input StRParen in
  let ty = begin match next_token input with
  | StColon ->
    let _ = expect_token input StColon in
    parse_ty input
  | _ -> TySyntax
  end in
  let body = parse_exp input in
  let _ = expect_token input StRParen in
  SLambda (ids, ty, body)

and parse_ty_lambda input =
  let ty = parse_ty input in
  let exp = parse_exp input in
  let _ = expect_token input StRParen in
  STyLambda (ty, exp)

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

and parse_define_type input =
  let e = SId (parse_id input) in
  let vars = parse_optional_ids input in
  let tys = parse_optional_exps input in
  let nxt = parse_exp input in
  let ne = SDefineTy (e, vars, tys, nxt) in
  ne

and parse_syntax_case input =
  let es = parse_cases input in
  let _ = expect_token input StRParen in
  (* print_endline "Parsed STXCASE"; *)
  SStxCase es

and parse_cases input =
  match next_token input with
  | StRParen -> []
  | StLBracket ->
    let _ = expect_token input StLBracket in
    let l = parse_exp input in
    let r = parse_exp input in
    let _ = expect_token input StRBracket in
    (l, r) :: parse_cases input

and parse_args input acc =
  let next = next_token input in
  match next with
  | StRParen -> acc
  | StLBracket ->
    let _ = expect_token input StLBracket in
    let e = SId (parse_id input) in
    begin
    match next_token input with
    | StColon ->
      let _ = expect_token input StColon in
      let ty = parse_ty input in
      let _ = expect_token input StRBracket in
      parse_args input (acc @ [(e, ty)])
    | StRBracket -> (* if no type - make syntax type *)
      let _ = expect_token input StRBracket in
      parse_args input (acc @ [(e, TySyntax)])
    end
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
  | StLBracket -> DSym "["
  | StRBracket -> DSym "]"
  | StColon -> DSym ":"
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
  | SLambda (args, ty, e) ->
    SOList [_rec (SId "lambda"); SOList (_args args); _ty ty; _rec e]
  | STyLambda (ty, e) ->
    SOList [_rec (SId "Lambda"); _ty ty; _rec e]
  | SLet (id, rhs, e) ->
    SOList [_rec (SId "let"); _rec id; _rec rhs; _rec e]
  | SLetStx (id, rhs, e) ->
    SOList [_rec (SId "let-syntax"); _rec id; _rec rhs; _rec e]
  | SQuote d -> SOList [_rec (SId "quote"); datum_to_stx d]
  | SQuoteStx d -> SOList [_rec (SId "quote-syntax"); datum_to_stx d]
  | SQuoteStxObj s -> SOList [_rec (SId "quote-syntax-obj"); s]
  | SDefine (id, args, ty, e, nxt) ->
    SOList [_rec (SId "define"); SOList (_rec id :: _args args); _ty ty; _rec e; _rec nxt]
  | SDefineTy (id, vars, tys, nxt) ->
    SOList [_rec (SId "define-type"); _rec id; SOList (List.map _rec vars);
      SOList (List.map _rec tys); _rec nxt]
  | SStxCase es ->
    (* print_endline ("EXP_TO_STX STXCASE"); *)
    SOList (_rec (SId "syntax-case") :: List.map (fun (l, r) -> SOList [_rec l; _rec r]) es)

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
  | TySyntax -> stx_core (SId "Syntax")
  | TyVar id -> stx_core (SId id)
  | TyArray ty -> SOList [stx_core (SId "Array"); _rec ty]
  | TyVector tys -> SOList (stx_core (SId "Vector") :: List.map _rec tys)
  | TyFix ty -> SOList [stx_core (SId "Fix"); _rec ty]
  | TyForAll (v, ty) -> SOList [stx_core (SId "Forall"); stx_core (SId v); _rec ty]
  | TyFn (args, ret) -> SOList (stx_core (SId "->") :: List.map _rec args @ _rec ret :: [])

let has_id s exp =
  match s with
  | SO (SId id, _) when id = exp -> true
  | _ -> false

let is_id s = match s with
  | SO (SId _, _) -> true
  | _ -> false

let is_list s = match s with
  | SOList _ -> true
  | _ -> false

let is_quote s = has_id s "quote"
let is_quotestx s = has_id s "quote-syntax"
let is_quotestxobj s = has_id s "quote-syntax-obj"
let is_lambda s = has_id s "lambda"
let is_tylambda s = has_id s "Lambda"
let is_letstx s = has_id s "let-syntax"
let is_let s = has_id s "let"
let is_define s = has_id s "define"
let is_define_type s = has_id s "define-type"
let is_stxcase s = has_id s "syntax-case"

let rec stx_to_datum s =
  match s with
  | SO (SId id, _) -> DSym id
  | SOList ss -> DList (List.map stx_to_datum ss)

let rec stx_to_ty s =
  match s with
  | s when has_id s "Int" -> TyInt
  | s when has_id s "Bool" -> TyBool
  | s when has_id s "Void" -> TyVoid
  | s when has_id s "Char" -> TyChar
  | s when has_id s "Syntax" -> TySyntax
  | SOList (s :: [t]) when has_id s "Array" -> TyArray (stx_to_ty t)
  | SOList (s :: t) when has_id s "Vector" -> TyVector (List.map stx_to_ty t)
  | SOList (s :: [t]) when has_id s "Fix" -> TyFix (stx_to_ty t)
  | SOList (s :: v :: [t]) when has_id s "Forall" ->
    let TyVar id = stx_to_ty v in
    TyForAll (id, stx_to_ty t)
  | SOList (s :: t) when has_id s "->" ->
    let args = List.map stx_to_ty (rm_last t) in
    let ret = stx_to_ty (last t) in
    TyFn (args, ret)
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
    | s :: SOList args :: ty :: body :: [] when is_lambda s ->
      SLambda (_args args, _ty ty, _rec body)
    | s :: ty :: body :: [] when is_tylambda s ->
      STyLambda (_ty ty, _rec body)
    | s :: l :: rhs :: body :: [] when is_let s ->
      SLet (_rec l, _rec rhs, _rec body)
    | s :: l :: rhs :: body :: [] when is_letstx s ->
      SLetStx (_rec l, _rec rhs, _rec body)
    | s :: SOList (id :: args) :: ty :: body :: nxt :: [] when is_define s ->
      SDefine (_rec id, _args args, _ty ty, _rec body, _rec nxt)
    | s :: id :: SOList vars :: SOList tys :: nxt :: [] when is_define_type s ->
      SDefineTy (_rec id, List.map _rec vars, List.map _rec tys, _rec nxt)
    | s :: cases when is_stxcase s ->
      (* print_endline ("STX_TO_EXP STXCASE"); *)
      SStxCase (stx_to_cases cases)
    | s :: t ->
      (* print_endline ("STX_TO_EXP APP"); *)
      SApp (_rec s :: List.map _rec t)

and stx_to_args args =
  let _rec e = stx_to_args e in
  let _exp e = stx_to_exp e in
  let _ty e = stx_to_ty e in
  (* print_endline ("args: " ^ String.concat " " (List.map str_syntax args)); *)
  match args with
  | SOList (e::ty::[]) :: tl -> (_exp e, _ty ty) :: _rec tl
  | [] -> []

and stx_to_cases s =
  let _rec e = stx_to_cases e in
  let _exp e = stx_to_exp e in
  match s with
  | SOList [l; r] :: t -> (_exp l, _exp r) :: _rec t
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

let core_form_ids = ["lambda"; "let-syntax"; "quote"; "quote-syntax"; "syntax-case"]

let core_forms = List.map _id core_form_ids

let core_primitive_ids = ["datum->syntax"; "syntax->datum"; "syntax-e"; "list";
  "cons"; "first"; "second"; "third"; "fourth"; "rest"; "map";
  "+"; "-"; "*"; "/"; "%"; "let"; "nth"; "define"; "or"; "if"; "eq?";
  "and"; "begin"; "array-set!"; "array-ref"; "print"; "vector";
  "array"; "vector-ref"; "vector-set!"; "define-type"; "void"; "vector-length";
  "read"; "zero?"; "pos?"; "not"; ">" ; ">="; "<"; "<="; "while"; "neg?";
  "unless"; "inst"; "Syntax"; "Int"; "Bool"; "Vector"; "Array"; "->"; "Char";
  "Void"; "Lambda"; "..."]

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

let str_macro m = match m with
  | Var -> "Var"
  | MacroFn f -> "MacroFn"

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
  (* print_endline ("expand id: " ^ str_syntax s); *)
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
  (* print_endline ("expand: " ^ str_syntax stx); *)
  match stx with
  | SO (e, _) -> expand_id stx env
  | SOList es -> match es with
    | s :: t when is_quote s || is_quotestx s || is_quotestxobj s -> stx
    | s :: SOList args :: ty :: body :: [] when is_lambda s ->
      expand_lambda s args ty body env
    | s :: ty :: body :: [] when is_tylambda s ->
      expand_ty_lambda s ty body env
    | s :: l :: rhs :: body :: [] when is_let s ->
      expand_let s l rhs body env
    | s :: l :: rhs :: body :: [] when is_letstx s ->
      expand_let_stx s l rhs body env
    | s :: SOList (id :: args) :: ty :: body :: nxt :: [] when is_define s ->
      expand_define s id args ty body nxt env
    | s :: id :: SOList vars :: SOList tys :: nxt :: [] when is_define_type s ->
      expand_define_type s id vars tys nxt env
    | s :: cases when is_stxcase s -> expand_stx_case s cases env
    | s :: t when is_id s -> expand_id_app stx env
    | s :: t -> expand_app stx env

and expand_stx_case s cases env =
  (* print_endline ("EXPAND STXCASE"); *)
  (* Add bound vars from lhs? *)
  (* let
  let new_cases = List.map (fun (l, r) ->
    let
    let binding = scope () in
    add_binding id binding;
    env_extend env binding Var;
  ) cases; *)
  SOList (s :: cases)

and expand_lambda lam args ty body env =
  let sc = scope () in
  let new_args = List.map (fun arg ->
    match arg with
    | SOList [e;ty] -> SOList [add_scope e sc; ty]
  ) args in
  List.iter (fun e ->
    match e with
    | SOList [id; _] ->
      let binding = scope () in
      add_binding id binding;
      env_extend env binding Var;
  ) new_args;
  let e = expand (add_scope body sc) ~env:env in
  SOList [lam; SOList new_args; ty; e]

and expand_ty_lambda s ty body env =
  let sc = scope () in
  let new_ty = add_scope ty sc in
  let binding = scope () in
  add_binding new_ty binding;
  env_extend env binding Var;
  let e = expand (add_scope body sc) ~env:env in
  SOList [s; new_ty; e]

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
  (* print_endline ("EXPAND LET STX: " ^ str_syntax rhs); *)
  let value = eval_for_syntax_binding rhs env in
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

and expand_define_type define_id id vars tys nxt env =
  let sc = scope () in
  let new_id = add_scope id sc in
  let new_tys = SOList (List.map (fun ty -> add_scope ty sc) tys) in
  let SOList all_tys = new_tys in
  List.iter (fun e ->
    match e with
    | SO (_, _) as id
    | SOList (id::_) ->
      (* print_endline ("Expand_define_type: Adding binding for: " ^ str_syntax id); *)
      let binding = scope () in
      add_binding id binding;
      env_extend env binding Var;
    | _ -> macro_error (str_syntax e)
  ) (new_id :: all_tys);
  let new_next = expand (add_scope nxt sc) ~env:env in
  SOList [define_id; new_id; SOList vars; new_tys; new_next]

and expand_id_app stx env =
  let SOList (id::_) = stx in
  let binding = resolve id in
  (* print_endline ("expand_id_app: " ^ str_syntax stx); *)
  let value = if binding = 0 then Var else env_lookup env binding in
  match value with
  | MacroFn fn -> expand (apply_transformer fn stx) ~env:env
  | _ -> expand_app stx env

and expand_app stx env =
  let _expand s = expand s ~env:env in
  match stx with
  | SO _ -> _expand stx
  | SOList t -> SOList (List.map _expand t)

and eval_for_syntax_binding stx env =
  let so = expand stx ~env:env in
  eval_compiled (compile so)

(* TBD *)
and convert t =
  (* print_endline ("Converting: " ^ str_syntax t); *)
  match t with
  | SOList [SO (SId "lambda", _); SOList args; body] ->
    exp_to_stx (SLambda (convert_args args, TySyntax, stx_to_exp body))
  | s ->
    (* print_endline ("convert: " ^ str_syntax s); *)
    s
  | _ -> macro_error ("Converted to invalid syntax: " ^ str_syntax t)

and convert_args args =
  match args with
  | SOList [e;_;ty] :: tl -> (stx_to_exp e, stx_to_ty ty) :: convert_args tl
  | (SO _ as e) :: tl -> (stx_to_exp e, TySyntax) :: convert_args tl
  | [] -> []

and eval e tbl =
  (* print_endline ("eval : " ^ str_exp e); *)
  match e with
  | SId id -> Hashtbl.find tbl e
  | SQuote _ | SQuoteStx _ -> exp_to_stx e
  | SLambda (args, ty, body) ->
    exp_to_stx (SLambda (args, ty, stx_to_exp (eval body tbl)))
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
    let SOList (_::snd::_) = eval t tbl in
    snd
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
    let SLambda (args, ty, body) = stx_to_exp (eval fn tbl) in
    SOList (List.map (fun e ->
      List.iter (fun (arg, ty) -> Hashtbl.add tbl arg e) args;
      eval body tbl
    ) es)
  (* Primtive application, just process args *)
  | SApp (SId id::t) ->
    let id = stx_core (SId id) in
    SOList (id :: List.map (fun e -> eval e tbl) t)
  | _ -> macro_error ("Eval: cannot eval " ^ str_exp e)

and eval_stx_case stx cases =
  match cases with
  | (ptn, body) :: t when ptn_match ptn stx ->
    (* print_endline ("Pattern matched! : " ^ str_exp ptn); *)
    let env = get_ptn_mapping ptn stx in
    let res = transform body env in
    (* print_endline ("STX CASE RESULT: " ^ str_exp (stx_to_exp res)); *)
    (* Should evaluate? *)
    (* introduce (eval (stx_to_exp res) (Hashtbl.create 10)) *)
    introduce res
  | _ :: t -> eval_stx_case stx t
  | [] -> macro_error ("Eval Syntax Case: No matching patterns.")

and transform e env =
  match e with
  | SApp [] -> exp_to_stx e
  | SId id -> begin match List.mem_assoc e env with
    | true ->
      (* print_endline (id ^ " in env"); *)
      let (i, s) = List.assoc e env in
      (* print_endline ("stx: " ^ str_syntax s); *)
      if i = 1 then s else macro_error ("transform id: not level 1")
    | false -> exp_to_stx e
    end
  | SApp l ->
    (* print_endline ("TRANSFORM: " ^ str_exp e); *)
    transform_list l env
  | _ -> exp_to_stx e

and transform_list l env =
  match l with
  | h :: SId "..." :: [] ->
    (* print_endline ("tl: h :: ... :: []"); *)
    print_endline (str_exp h);
    if not (controllable h env) then
      macro_error ("transform (e ...): not controllable")
    else
      let env' = _combine (decompose h env) 0 in
      (* print_env env'; *)
      SOList (List.map (fun e -> transform h e) env')
  | h :: t :: [] ->
    (* print_endline ("tl: h :: t :: []"); *)
    SOList [transform h env; transform t env]
  | h :: t ->
    let SOList res = transform_list t env in
    SOList (transform h env :: res)

and print_env env' =
  List.iter (fun e ->
    print_endline "env: ";
    (List.iter (fun (k, (i, s)) ->
      print_endline ("(" ^ str_exp k ^ " (" ^ string_of_int i ^ ", " ^ str_syntax s ^ ")")) e)
  ) env'

and _combine l n =
  let open List in
  if n > length l then []
  else map (fun e -> nth e n) l :: _combine l (n + 1)

and decompose p env =
  let open List in
  let vars = fv p in
  (* print_endline ("Decompose pattern:" ^ str_exp p); *)
  map (fun v ->
    (* print_endline ("Decompose:" ^ str_exp v); *)
    let (n, s) = List.assoc v env in
    (* print_endline ("(n, s): " ^ string_of_int n ^ " " ^  str_syntax s); *)
    let SOList ss = s in
    if n = 1 then
      (v, (n, s)) :: map (fun _ -> (v, (n, s))) ss
    else
      map (fun sj -> (v, (n - 1, sj))) ss
  ) vars

and controllable p env =
  let vars = fv p in
  (* print_endline ("CONTROLLABLE VARIABLES:"); *)
  (* List.iter (fun e -> print_endline (str_exp e)) vars; *)
  List.exists (fun v ->
    (* print_endline ("Controllable:" ^ str_exp v); *)
    let (i, _) = List.assoc v env in
    i > 1
  ) vars

and fv p =
  let open List in
  let _rec e = fv e in
  match p with
  | SId "_" | SId "..." -> []
  | SId id when not (mem id core_primitive_ids) -> [p]
  | SApp l -> concat (map _rec l)
  | _ -> []

and get_ptn_mapping ptn stx =
  match ptn with
  | SApp [] | SId "_" -> []
  | SId _ -> [(ptn, (1, stx))]
  | SQuote _ -> [(ptn, (1, stx))]
  | SApp l -> get_ptn_mapping_list l stx

and get_ptn_mapping_list l stx =
  let open List in
  let SOList sos = stx in
  (* print_endline ("get ptn match list: " ^ String.concat " " (List.map str_exp l)); *)
  match l with
  | h :: SId "..." :: [] ->
    let res = concat (map (fun s -> get_ptn_mapping h s) sos) in
    let (k, (i, _)) = hd res in
    [(k, (i + 1, SOList (map (fun (_, (_, s)) -> s) res)))]
    (* map (fun (k, (i, s)) -> (k, (i + 1, s))) res *)
  | h :: t ->
    let tl = get_ptn_mapping_list t (SOList (tl sos)) in
    get_ptn_mapping h (hd sos) @ tl
  | [] -> []

and ptn_match ptn stx =
  match ptn with
  | SApp [] -> stx = SOList []
  (* Vars, numbers, bool match  *)
  | SId _ -> true
  | SQuote _ -> ptn = stx_to_exp stx
  (* Map stx over first pattern  *)
  | SApp l -> ptn_match_list l stx

and ptn_match_list l stx =
  let open List in
  if is_list stx then
    let SOList sos = stx in
    match l with
    | h :: SId "..." :: [] ->
      for_all (fun s -> ptn_match h s) sos
    | h :: t :: [] ->
      ptn_match h (hd sos) && ptn_match t (hd (tl sos))
    | h :: t ->
      ptn_match h (hd sos) && ptn_match_list t (SOList (tl sos))
  else false

and eval_compiled exp =
  MacroFn (fun stx ->
    match exp with
    | SLambda (args, ty, body) ->
      let binding = Hashtbl.create 10 in
      List.iter (fun (arg, ty) -> Hashtbl.add binding arg stx) args;
    (* introduce *) eval body binding
    | SStxCase es ->
      (* print_endline ("Eval Compiled: " ^ str_exp exp); *)
      eval_stx_case stx es
    | _ -> macro_error ("Eval_compiled: expected lambda, but received: " ^ str_exp exp)
  )

(* TBD *)
and compile stx =
  let open List in
  let _rec s = compile s in
  match stx with
  | SO (SId id, _) when mem id core_primitive_ids || mem id core_form_ids -> SId id
  | SO (SId "_", _) -> SId "_"
  | SO (SId id, _) ->
    (* print_endline ("resolving: " ^ id); *)
    let binding = resolve stx in
    SId (id ^ "_" ^ string_of_int binding)
  | SOList es -> match es with
    | s :: t when is_quote s -> SQuote (stx_to_datum (List.hd t))
    | s :: t when is_quotestx s -> SQuoteStxObj (List.hd t)
    | s :: t when is_quotestxobj s -> SQuoteStxObj (List.hd t)
    | s :: SOList args :: ty :: body :: [] when is_lambda s -> SLambda (compile_args args, stx_to_ty ty, _rec body)
    | s :: l :: rhs :: body :: [] when is_let s ->
      SLet (_rec l, _rec rhs, _rec body)
    | s :: l :: rhs :: body :: [] when is_letstx s ->
      SLetStx (_rec l, _rec rhs, _rec body)
    | s :: SOList (id :: args) :: ty :: body :: nxt :: [] when is_define s ->
      (* print_endline "compile define"; *)
      SDefine (_rec id, compile_args args, stx_to_ty ty, _rec body, _rec nxt)
    | s :: cases when is_stxcase s ->
      SStxCase (List.map (fun (SOList [l;r]) -> (_rec l, _rec r)) cases)
    | s :: t -> SApp (_rec s :: List.map _rec t)

and compile_args args =
  match args with
  | SOList [e;ty] :: tl -> (compile e, stx_to_ty ty) :: compile_args tl
  | [] -> []

let expand_macros filename =
  let program = filename in
  let stream  = get_stream program `File in
  let tokens  = lex stream in
  let ast     = parse (ref tokens) in
  let out     = stx_to_exp @@ expand @@ introduce @@ exp_to_stx ast in
  str_exp out

let () =
  if Array.length Sys.argv = 1 then
    print_endline "Usage: ./main.native {filename}"
  else
    let str = expand_macros Sys.argv.(1) in
    print_endline str
