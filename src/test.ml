open Gensym
open Macro_ast
open Syntax_objects
open Binding

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
  let sc = Gensym.gen_int () in
  let ad = adjust_scope so sc add_scope in
  print_endline ("\nTEST: Add_Scope");
  print_endline ("<-: " ^ str_syntax so);
  print_endline ("->: " ^ str_syntax ad)

let test_flip_scope () =
  let sc = Gensym.gen_int () in
  let ad = adjust_scope so sc add_scope in
  let fl = adjust_scope ad sc flip_scope in
  print_endline ("\nTEST: Flip_Scope");
  print_endline ("<-: " ^ str_syntax ad);
  print_endline ("->: " ^ str_syntax fl)

let test_resolve () =
  print_endline ("\nTEST: Resolve");
  (* Add empty scoped SyntaxObject *)
  let binding = Gensym.gen_int () in
  let so      = SyntaxObj(SId "x", ScopeSet.empty) in
  add_binding so binding;
  print_endline ("Assigned " ^ str_syntax so
                  ^ " binding of " ^ string_of_int binding);

  (* Add one scoped SyntaxObject *)
  let sc  = Gensym.gen_int () in
  let so2 = adjust_scope so sc add_scope in

  (* Add two scoped SyntaxObject *)
  let sc2 = Gensym.gen_int () in
  let so3 = adjust_scope so2 sc2 add_scope in

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
  print_endline ("->: " ^ string_of_int loc);
