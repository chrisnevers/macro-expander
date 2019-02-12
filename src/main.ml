open List

open Macro_token
open Macro_ast
open Macro_lex
open Macro_parse
open Binding
open Expander
open Test

let print_ast ast =
  print_endline ("\nPARSED: ");
  print_endline @@ String.concat "\n" (map str_s_exp ast)

(* Driver *)
let () =
  (* let program = Sys.argv.(1) in *)
  let program = "examples/ex.rkt" in
  let stream  = get_stream program `File in
  let tokens  = lex stream in
  let ast     = parse (ref tokens) in

  print_ast ast;

  test_datum_to_syntax ();
  test_syntax_to_datum ();
  test_add_scope ();
  test_flip_scope ();
  test_resolve ();
  test_resolve_false ();
  test_core ();
  test_core_fail ();
  test_introduce ();
  test_expand_id ();
  test_expand_app ();
