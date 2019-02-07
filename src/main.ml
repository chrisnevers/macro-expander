open List

open Macro_token
open Macro_ast
open Macro_lex
open Macro_parse
open Binding
open Test

let print_ast ast =
  print_endline ("\nPARSED: ");
  print_endline @@ String.concat "\n" (map str_s_exp ast)

(* Driver *)
let () =
  let program = Sys.argv.(1) in
  let stream  = get_stream program `File in
  let tokens  = lex stream in
  let ast     = parse (ref tokens) in

  print_ast ast;

  test_datum_to_syntax ();
  test_syntax_to_datum ();
  test_add_scope ();
  test_flip_scope ();
  test_resolve ();
