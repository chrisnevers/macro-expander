open List

open Macro_token
open Macro_ast

exception ParserError of string
let parser_error msg = raise (ParserError msg)

let next_token tokens = hd !tokens

let get_token tokens =
  let token = next_token tokens in
  tokens := tl !tokens;
  token

let expect_token tokens expected =
  let actual = get_token tokens in
  if actual != expected then
    parser_error ("Expected " ^ str_s_token expected ^ ", but received: "
                  ^ str_s_token actual)
  else ()

let parse_id input =
  match get_token input with
  | StId id -> id
  | _ -> parser_error "expected identifier"

let rec parse_optional_exps input =
  let next = next_token input in
  match next with
  | StRParen -> []
  | _ -> let e = parse_exp input in
    e :: parse_optional_exps input

and parse_exp input =
  let token = get_token input in
  match token with
  | StLParen ->
    let exp = parse_inside_paren input in
    let _ = expect_token input StRParen in exp
  | StId id -> SId id
  | StQuote -> SQuote (parse_datum input)
  | _ -> parser_error ("unexpected exp: " ^ str_s_token token)

and parse_inside_paren input =
  let token = get_token input in
  match token with
  | StRParen -> parser_error "not expecting: ()"
  | StId "let-syntax" -> parse_let_syntax input
  | StId "quote" -> parse_quote input
  | StId "quote-syntax" -> parse_quote_syntax input
  | StId "lambda" -> parse_lambda input
  | StId id ->
    let exps = parse_optional_exps input in
    SApply (SId id, exps)
  | _ -> parser_error ("did not expect " ^ str_s_token token ^ " in (")

and parse_let_syntax input =
  let _ = expect_token input StLParen in
  let _ = expect_token input StLBracket in
  let id = parse_id input in
  let pattern = parse_exp input in
  let _ = expect_token input StRBracket in
  let _ = expect_token input StRParen in
  let body = parse_exp input in
  SLetSyntax (id, pattern, body)

and parse_quote input = SQuote (parse_datum input)

and parse_quote_syntax input = SQuoteSyntax (parse_datum input)

and parse_lambda input =
  let _ = expect_token input StLParen in
  let id = parse_id input in
  let _ = expect_token input StRParen in
  let body = parse_exp input in
  SLambda (id, body)

and parse_datums input =
  let next = next_token input in
  match next with
  | StRParen -> []
  | _ -> let d = parse_datum input in
    d :: parse_datums input

and parse_datum input =
  let token = get_token input in
  match token with
  | StId id -> SSymbol id
  | StLParen ->
    let ds = parse_datums input in
    let _ = expect_token input StRParen in
    SList ds
  | StQuote -> SList [SSymbol "quote"; parse_datum input]
  | _ -> parser_error ("parse_datum: " ^ str_s_token token)

let rec parse input =
  let next = next_token input in
  match next with
  | StEOF -> []
  | _ -> let e = parse_exp input in e :: parse input
