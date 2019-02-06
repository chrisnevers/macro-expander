open Stream
open Char

open Macro_token

exception LexerError of string
let lexer_error msg = raise (LexerError msg)

let get_stream src ty = match ty with
  | `File -> of_channel @@ open_in src
  | `String -> of_string src

let is_digit c =
  let asci = code c in
  asci >= code('0') && asci <= code('9')

let is_alpha c =
  let asci = code c in
  (asci >= code('A') && asci <= code('Z')) ||
  (asci >= code('a') && asci <= code('z'))

let is_space c = c = ' ' || c = '\n' || c = '\t'

let is_id c = match c with
  | '?' | '!' | '\'' | '#' | '$' | '-' | '_' -> true
  | _ when is_alpha c || is_digit c -> true
  | _ -> false

let peek_char stream = peek stream

let rec skip_line stream =
  let c = next stream in
  match c with
  | '\n' -> next_char stream
  | _ -> skip_line stream

and next_char stream = try
  let c = next stream in
  match c with
  | s when is_space c -> next_char stream
  | ';' -> skip_line stream
  | c -> Some c
  with Failure -> None

let rec scan_id stream acc =
  let c = peek_char stream in
  match c with
  | Some c when is_id c ->
    let _ = next_char stream in
    scan_id stream (acc ^ (escaped c))
  | _ -> match acc with
    | _ -> StId acc

let scan_token stream =
  match next_char stream with
  | None -> StEOF
  | Some '(' -> StLParen
  | Some ')' -> StRParen
  | Some '[' -> StLBracket
  | Some ']' -> StRBracket
  | Some '\'' -> StQuote
  | Some c -> scan_id stream (escaped c)

let rec lex program =
  let token = scan_token program in
  match token with
  | StEOF -> token :: []
  | _ -> token :: lex program
