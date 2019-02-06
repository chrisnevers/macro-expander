open List

open Macro_token
open Macro_ast
open Macro_lex
open Macro_parse
open Syntax_objects

let print_ast ast = print_endline @@ String.concat "\n" (map str_s_exp ast)

(* Driver *)
let () =
  let program = Sys.argv.(1) in
  let stream  = get_stream program `File in
  let tokens  = lex stream in
  let ast     = parse (ref tokens) in

  (* Prints parsed AST *)
  print_ast ast;

  (* Tests syntax->datum & datum->syntax *)
  match ast with
  | SQuote d :: t ->
    let syntax_objs = datum_to_syntax d in
    let str = String.concat " " (map str_syntax syntax_objs) in
    print_endline ("Datum_to_syntax: " ^ str);
    let datum = syntax_to_datum syntax_objs in
    let str = str_s_exp datum in
    print_endline ("Syntax_to_datum: " ^ str);
  | _ -> ()
