open List

open Macro_token
open Macro_ast
open Macro_lex
open Macro_parse

(* Driver *)
let () =
  let program = Sys.argv.(1) in
  let stream = get_stream program `File in
  let tokens = lex stream in
  let ast = parse (ref tokens) in
  print_endline @@ String.concat "\n" (map str_s_exp ast)
