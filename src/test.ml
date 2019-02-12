open Gensym
open Macro_ast
open Syntax_objects
open Binding
open Scope
open Expander

let so = SyntaxList [SyntaxObj(SQuote (SSymbol "a"), ScopeSet.empty);
          SyntaxList [SyntaxObj(SQuote (SSymbol "b"), ScopeSet.empty)]]

let e = SQuote (SList [SSymbol "a"; SList [SSymbol "b"]])

let test_datum_to_syntax () =
  let so = datum_to_syntax @@ get_datum e in
  print_endline ("\nTEST: Datum->Syntax");
  print_endline ("<-: " ^ str_s_exp e);
  print_endline ("->: " ^ str_syntax so)

let test_syntax_to_datum () =
  let e = syntax_to_datum so in
  print_endline ("\nTEST: Syntax->Datum");
  print_endline ("<-: " ^ str_syntax so);
  print_endline ("->: " ^ str_s_exp e)

let test_add_scope () =
  let sc = scope () in
  let ad = adjust_scope so sc add_scope in
  print_endline ("\nTEST: Add_Scope");
  print_endline ("<-: " ^ str_syntax so);
  print_endline ("->: " ^ str_syntax ad)

let test_flip_scope () =
  let sc = scope () in
  let ad = adjust_scope so sc add_scope in
  let fl = adjust_scope ad sc flip_scope in
  print_endline ("\nTEST: Flip_Scope");
  print_endline ("<-: " ^ str_syntax ad);
  print_endline ("->: " ^ str_syntax fl)

let test_resolve () =
  print_endline ("\nTEST: Resolve");
  (* Add empty scoped SyntaxObject *)
  let binding = Gensym.gen_int () in
  let id = SId "x" in
  let so = SyntaxObj(id, ScopeSet.empty) in
  add_binding so binding;
  print_endline ("Assigned " ^ str_syntax so
                  ^ " binding of " ^ string_of_int binding);

  (* Add one scoped SyntaxObject *)
  let sc  = scope () in
  let so2 = syntax id [sc] in

  (* Add two scoped SyntaxObject *)
  let sc2 = scope () in
  let so3 = syntax id [sc; sc2] in

  (* Add one scoped SO to binding table *)
  let binding2 = Gensym.gen_int () in
  add_binding so2 binding2;
  print_endline ("Assigned " ^ str_syntax so2
                  ^ " binding of " ^ string_of_int binding2);

  (* Add two scoped SO to binding table *)
  let binding3 = Gensym.gen_int () in
  add_binding so3 binding3;
  print_endline ("Assigned " ^ str_syntax so3
                  ^ " binding of " ^ string_of_int binding3);

  let loc = resolve so3 in
  print_endline ("<-: " ^ str_syntax so3);
  print_endline ("->: " ^ string_of_int loc);

  let loc = resolve so2 in
  print_endline ("<-: " ^ str_syntax so2);
  print_endline ("->: " ^ string_of_int loc);

  let loc = resolve so in
  print_endline ("<-: " ^ str_syntax so);
  print_endline ("->: " ^ string_of_int loc)

let test_resolve_false () =
  (* Hashtbl.reset all_bindings; *)
  print_endline ("\nTEST: Resolve (False)");
  let sc1 = scope () in
  let so = SyntaxObj(SId "y", ScopeSet.empty) in
  let so1 = adjust_scope so sc1 add_scope in
  (* Add pink binding *)
  let binding = Gensym.gen_int () in
  add_binding so1 binding;
  print_endline ("Assigned " ^ str_syntax so1
                  ^ " binding of " ^ string_of_int binding);
  (* Add pink and orange binding *)
  let sc2 = scope () in
  let so2 = adjust_scope so1 sc2 add_scope in
  let binding2 = Gensym.gen_int () in
  add_binding so2 binding2;
  print_endline ("Assigned " ^ str_syntax so2
                  ^ " binding of " ^ string_of_int binding2);
  (* Lookup just orange binding *)
  let so3 = adjust_scope so sc2 add_scope in
  let loc = resolve so3 in
  print_endline ("<-: " ^ str_syntax so3);
  print_endline ("->: " ^ string_of_int loc)

let test_core () =
  print_endline ("\nTEST: Core");
  let so = adjust_scope (mk_syntax (SId "cons")) core_scope add_scope in
  let loc = resolve so in
  print_endline ("<-: " ^ str_syntax so);
  print_endline ("->: " ^ string_of_int loc)

let test_core_fail () =
  print_endline ("\nTEST: Core (False)");
  let so = mk_syntax (SId "cons") in
  let loc = resolve so in
  print_endline ("<-: " ^ str_syntax so);
  print_endline ("->: " ^ string_of_int loc)

let test_introduce () =
  print_endline ("\nTEST: Introduce");
  let so = datum_to_syntax (SSymbol "cons") in
  let res = introduce so in
  print_endline ("<-: " ^ str_syntax so);
  print_endline ("->: " ^ str_syntax res)

let test_expand_id () =
  print_endline ("\nTEST: Expand Id");

  (* Test primitive id *)
  print_string "Expand id (primitive): ";
  let so = mk_syntax (SId "list") in
  expand (introduce so);

  (* Test core form id *)
  print_string "Expand id (form): ";
  let so = mk_syntax (SId "lambda") in
  try expand (introduce so) with ExpandError _ -> print_endline "bad syntax (good)";

  (* Test variable with binding *)
  print_string "Expand id (var): ";
  let so = mk_syntax (SId "x") in
  let binding = scope () in
  add_binding so binding;
  let empty_env = Hashtbl.create 10 in
  env_extend empty_env binding Variable;
  expand_id (introduce so) empty_env

let test_expand_app () =
  print_endline ("\nTEST: Expand App");
  print_string "Expand app: ";
  let sc = scope () in
  let so = SyntaxList [syntax (SId "f") [sc]; SyntaxList []] in
  let binding = scope () in
  add_binding (syntax (SId "f") [sc]) binding;
  let empty_env = Hashtbl.create 10 in
  env_extend empty_env binding MacroFunction;
  expand_id_app (introduce so) empty_env;


  print_string "Expand app: ";
  expand (mk_syntax (SApply (SId "one", [])));

  print_string "Expand lambda: ";
  expand (mk_syntax (SLambda ("x", SId "x")));

  print_string "Expand list: ";
  expand (mk_syntax (SApply (SApply (SId "curried", [SQuote (SSymbol "1")]), [SQuote (SSymbol "2")])))
