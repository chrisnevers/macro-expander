open OUnit
open Main

let mt = ScopeSet.empty

let test_get_datum = fun () ->
  let expected = DSym "5" in
  let actual = get_datum (SQuote expected) in
  assert_equal actual expected

let test_get_datum_exn = fun () ->
  let fn = fun () -> get_datum (SId "5") in
  assert_raises ~msg:"Throws error if not SQuote OR SQouteStx"
    (MacroError "Get_datum: Expected SQuote or SQuoteStx, but received: 5") fn

let test_stx_list = fun () ->
  let expected = SO (SId "x", ScopeSet.of_list [1]) in
  let actual = stx_list (SId "x") [1] in
  assert_equal actual expected

let test_exp_to_stx_id = fun () ->
  let expected = SO (SId "x", mt) in
  let actual = exp_to_stx (SId "x") in
  assert_equal actual expected

let test_exp_to_stx_app = fun () ->
  let expected = SOList [SO (SId "list", mt);
    SO (SId "1", mt)] in
  let actual = exp_to_stx (SApp [SId "list"; SId "1"]) in
  assert_equal actual expected

let test_exp_to_stx_lambda = fun () ->
  let expected = SOList [
    SO (SId "lambda", mt);
    SO (SId "x", mt);
    SO (SId "x", mt)] in
  let actual = exp_to_stx (SLambda (SId "x", SId "x")) in
  assert_equal actual expected

let test_exp_to_stx_let_stx = fun () ->
  let expected = SOList [
    SO (SId "let-syntax", mt);
    SO (SId "x", mt);
    exp_to_stx (SLambda (SId "x", SId "x"));
    exp_to_stx (SApp [SId "x"])
  ] in
  let actual = exp_to_stx (
    SLetStx (SId "x", SLambda (SId "x", SId "x"), SApp [SId "x"])
  ) in
  assert_equal actual expected

let test_exp_to_stx_quote = fun () ->
  let expected = SOList [
    SO (SId "quote", mt);
    SO (SId "1", mt)
  ] in
  let actual = exp_to_stx (SQuote (DSym "1")) in
  assert_equal actual expected

let test_exp_to_stx_quote_stx = fun () ->
  let expected = SOList [SO (SId "quote-syntax", mt); SO (SId "lambda", mt)] in
  let actual = exp_to_stx (SQuoteStx (DSym "lambda")) in
  assert_equal actual expected

let test_exp_to_stx_quote_stx_obj = fun () ->
  let so = stx_list (SId "x") [1] in
  let expected = SOList [SO (SId "quote-syntax-obj", mt); so] in
  let actual = exp_to_stx (SQuoteStxObj so) in
  assert_equal actual expected

let test_stx_to_exp_quote = fun () ->
  let quote = (SQuote (DList [DSym "1"; DSym "2"])) in
  let actual = stx_to_exp (exp_to_stx quote) in
  assert_equal actual quote

let test_stx_to_exp_quote_stx = fun () ->
  let quote = (SQuoteStx (DSym "'")) in
  let actual = stx_to_exp (exp_to_stx quote) in
  assert_equal actual quote

let test_stx_to_exp_quote_stx_obj = fun () ->
  let quote = (SQuoteStxObj (SOList [SO (SId "x", mt)])) in
  let actual = stx_to_exp (exp_to_stx quote) in
  assert_equal actual quote

let test_stx_to_exp_lambda = fun () ->
  let expected = SLambda (SId "x", SApp [SId "list"; SId "x"]) in
  let actual = stx_to_exp (exp_to_stx expected) in
  assert_equal actual expected

let test_stx_to_exp_let_stx = fun () ->
  let expected = SLetStx (SId "x", SLambda (SId "x", SId "x"), SApp [SId "x"]) in
  let actual = stx_to_exp (exp_to_stx expected) in
  assert_equal actual expected

let test_stx_to_exp_app = fun () ->
  let expected = SApp [SApp [SId "list"; SId "1"]; SId "x"] in
  let actual = stx_to_exp (exp_to_stx expected) in
  assert_equal actual expected

let test_flip_scope_add = fun () ->
  let open ScopeSet in
  let expected = SO (SId "x", of_list [1]) in
  let actual = flip_scope (SO (SId "x", mt)) 1 in
  assert_equal actual expected

let test_flip_scope_remove = fun () ->
  let open ScopeSet in
  let expected = SO (SId "x", mt) in
  let actual = flip_scope (SO (SId "x", of_list [1])) 1 in
  assert_equal actual expected

let test_add_scope = fun () ->
  let open ScopeSet in
  let expected = SO (SId "x", of_list [1]) in
  let actual = flip_scope (SO (SId "x", mt)) 1 in
  assert_equal actual expected

let test_resolve_one = fun () ->
  let open ScopeSet in
  let so = SO (SId "a", of_list [1]) in
  let expected = 1 in
  Hashtbl.add all_bindings so 1;
  let actual = resolve so in
  assert_equal actual expected

let test_resolve_two = fun () ->
  let open ScopeSet in
  Hashtbl.clear all_bindings;
  let bound = SO (SId "a", of_list [1]) in
  let so = SO (SId "a", of_list [1; 2]) in
  let expected = 1 in
  Hashtbl.add all_bindings bound 1;
  let actual = resolve so in
  assert_equal actual expected

let test_resolve_three = fun () ->
  let open ScopeSet in
  Hashtbl.clear all_bindings;
  let bound = SO (SId "a", of_list [1]) in
  let so = SO (SId "a", of_list [2]) in
  let expected = - 1 in
  Hashtbl.add all_bindings bound 1;
  let actual = resolve so in
  assert_equal actual expected

let test_resolve_four = fun () ->
  let open ScopeSet in
  Hashtbl.clear all_bindings;
  let bound_1 = SO (SId "b", of_list [1]) in
  let bound_2 = SO (SId "b", of_list [1; 2]) in
  let so = SO (SId "b", of_list [2]) in
  Hashtbl.add all_bindings bound_1 1;
  Hashtbl.add all_bindings bound_2 2;
  assert_equal 1 (resolve bound_1);
  assert_equal 2 (resolve bound_2);
  assert_equal (-1) (resolve so)

let test_resolve_five = fun () ->
  let open ScopeSet in
  Hashtbl.clear all_bindings;
  let bound_1 = SO (SId "c", of_list [1]) in
  let bound_2 = SO (SId "c", of_list [2]) in
  let so = SO (SId "c", of_list [1; 2]) in
  Hashtbl.add all_bindings bound_1 1;
  Hashtbl.add all_bindings bound_2 2;
  assert_equal 1 (resolve bound_1);
  assert_equal 2 (resolve bound_2);
  let fn = fun () -> resolve so in
  assert_raises ~msg:"Throws error if not ambiguous stx"
    (MacroError "Check_ambigious: ambigious scope: (StxObj c {1})") fn

let test_introduce = fun () ->
  let open ScopeSet in
  let expected = SOList [SO (SId "first", of_list [0]); SO (SId "stx", of_list [0])] in
  let actual = introduce (SOList [stx_mt (SId "first"); stx_mt (SId "stx")]) in
  assert_equal actual expected

let test_expand_id_core_primitive = fun () ->
  let env = Hashtbl.create 10 in
  add_core ();
  let so = introduce (SO (SId "list", mt)) in
  let actual = expand_id so env in
  assert_equal actual so

let test_expand_id_core_form = fun () ->
  let env = Hashtbl.create 10 in
  add_core ();
  let so = introduce (SO (SId "lambda", mt)) in
  let fn = fun () -> expand_id so env in
  assert_raises ~msg:"Throws error if core form"
    (MacroError "Expand_id: Bad syntax. Use of core form: (StxObj lambda {0})") fn

let test_expand_id_variable = fun () ->
  let open ScopeSet in
  let env = Hashtbl.create 10 in
  let binding = scope () in
  let so = SO (SId "x", of_list [0; binding]) in
  add_binding so binding;
  env_extend env binding Var;
  let actual = expand_id so env in
  assert_equal actual so

let test_expand_id_other = fun () ->
  let open ScopeSet in
  let env = Hashtbl.create 10 in
  let binding = scope () in
  let so = SO (SId "x", of_list [0; binding]) in
  add_binding so binding;
  env_extend env binding (MacroFn (fun s -> s));
  let fn = fun () -> expand_id so env in
  assert_raises ~msg:"Throws error if not a variable"
    (MacroError "Expand_id: Bad syntax: (StxObj x {0 2})") fn

let test_expand_lambda = fun () ->
  let open ScopeSet in
  let so = introduce (SOList [SO (SId "lambda", mt); SO (SId "x", mt);
    SO (SId "x", mt)]) in
  let actual = expand so in
  let expected = SOList [SO (SId "lambda", of_list [0]);
    SO (SId "x", of_list [0; 3]); SO (SId "x", of_list [0; 3])] in
  assert_equal actual expected

let test_expand_id_app_macro = fun () ->
  let open ScopeSet in
  let env = Hashtbl.create 10 in
  (* Add variable binding *)
  let binding = scope () in
  let so = SO (SId "x", of_list [0; binding]) in
  add_binding so binding;
  env_extend env binding Var;
  (* Add macro to env *)
  let id_so = SO (SId "id", of_list [1]) in
  let so_app = SOList [id_so] in
  add_binding id_so 1;
  env_extend env 1 (MacroFn (fun e -> so));
  (* Expected macro function result with new scope added *)
  let actual = expand so_app ~env:env in
  let expected = SO (SId "x", of_list [0; binding; binding + 1]) in
  assert_equal actual expected

let test_expand_id_app_var = fun () ->
  let open ScopeSet in
  let env = Hashtbl.create 10 in
  (* Add macro to env *)
  let id_so = SO (SId "id", of_list [1]) in
  let so_app = SOList [id_so] in
  add_binding id_so 1;
  env_extend env 1 Var;
  (* Expected macro function result with new scope added *)
  let actual = expand so_app ~env:env in
  assert_equal actual so_app

let test_expand_app = fun () ->
  let open ScopeSet in
  let env = Hashtbl.create 10 in
  (* Add macro to env *)
  let id_so = SO (SId "id", of_list [1]) in
  let so_app = SOList [SOList [id_so]] in
  add_binding id_so 1;
  env_extend env 1 Var;
  (* Expected macro function result with new scope added *)
  let actual = expand so_app ~env:env in
  assert_equal actual so_app

let test_expand_let_stx = fun () ->
  let so = introduce @@ exp_to_stx (SLetStx (SId "x", SLambda (SId "stx",
    SQuote (DSym "1")), SApp [SId "x"])) in
  let actual = expand so in
  assert_equal (stx_to_exp actual) (SQuote (DSym "1"))

let test_expand_let_stx_list = fun () ->
  let so = introduce @@ exp_to_stx (SLetStx (SId "x", SLambda (SId "stx",
    SApp [SId "list"; SQuoteStx (DSym "lambda"); SApp [SId "list"; SQuoteStx (DSym "x")];
    SApp [SId "second"; SId "stx"]]), SApp [SId "x"; SQuote (DSym "1")])) in
  let actual = expand so in
  (* print_endline ("Actual: " ^ str_exp (stx_to_exp actual));
  print_endline ("Actual: " ^ str_syntax actual); *)
  assert_equal (stx_to_exp actual) (SLambda (SId "x", SQuote (DSym "1")))

let test_expand_let_stx_rest = fun () ->
  let so = introduce @@ exp_to_stx (SLetStx (SId "x", SLambda (SId "stx",
    SApp [SId "list"; SQuoteStx (DSym "lambda"); SApp [SId "list"; SQuoteStx (DSym "x")];
    SApp [SId "rest"; SId "stx"]]), SApp [SId "x"; SQuote (DSym "1"); SQuote (DSym "2")])) in
  let actual = expand so in
  assert_equal (stx_to_exp actual) (SLambda (SId "x", SApp [SQuote (DSym "1"); SQuote (DSym "2")]))

let test_expand_let_stx_map = fun () ->
  let so = introduce @@ exp_to_stx (SLetStx (SId "x", SLambda (SId "stx",
    SApp [SId "map"; SLambda (SId "e", SQuote (DSym "booyah"));
      SQuote (DList [DSym "1"; DSym "2"])]),
    SApp [SId "x"])) in
  let actual = expand so in
  assert_equal (stx_to_exp actual) (SApp [SQuote (DSym "booyah"); SQuote (DSym "booyah")])

let suite =
  "Tests" >:::
  [
    "get_datum" >:: test_get_datum;
    "get_datum_exn" >:: test_get_datum_exn;
    "stx_list" >:: test_stx_list;
    "exp_to_stx SId" >:: test_exp_to_stx_id;
    "exp_to_stx SApp" >:: test_exp_to_stx_app;
    "exp_to_stx SLambda" >:: test_exp_to_stx_lambda;
    "exp_to_stx SLetStx" >:: test_exp_to_stx_let_stx;
    "exp_to_stx SQuote" >:: test_exp_to_stx_quote;
    "exp_to_stx SQuoteStx" >:: test_exp_to_stx_quote_stx;
    "exp_to_stx SQuoteStxObj" >:: test_exp_to_stx_quote_stx_obj;
    "stx_to_exp SQuote" >:: test_stx_to_exp_quote;
    "stx_to_exp SQuoteStx" >:: test_stx_to_exp_quote_stx;
    "stx_to_exp SQuoteStxObj" >:: test_stx_to_exp_quote_stx_obj;
    "stx_to_exp SLambda" >:: test_stx_to_exp_lambda;
    "stx_to_exp SLetStx" >:: test_stx_to_exp_let_stx;
    "stx_to_exp SApp" >:: test_stx_to_exp_app;
    "flip_scope add" >:: test_flip_scope_add;
    "flip_scope remove" >:: test_flip_scope_remove;
    "add_scope" >:: test_add_scope;
    "resolve one" >:: test_resolve_one;
    "resolve two" >:: test_resolve_two;
    "resolve three" >:: test_resolve_three;
    "resolve four" >:: test_resolve_four;
    "resolve five" >:: test_resolve_five;
    "introduce" >:: test_introduce;
    "expand_id core primitive" >:: test_expand_id_core_primitive;
    "expand_id core form" >:: test_expand_id_core_form;
    "expand_id variable" >:: test_expand_id_variable;
    "expand_id other" >:: test_expand_id_other;
    "expand lambda" >:: test_expand_lambda;
    "expand id app macro" >:: test_expand_id_app_macro;
    "expand id app var" >:: test_expand_id_app_var;
    "expand app" >:: test_expand_app;
    "expand let syntax" >:: test_expand_let_stx;
    "expand let syntax list" >:: test_expand_let_stx_list;
    "expand let syntax rest" >:: test_expand_let_stx_rest;
    "expand let syntax map" >:: test_expand_let_stx_map;
  ]

let _ = run_test_tt_main suite
