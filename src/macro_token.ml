type s_token =
  | StLParen
  | StRParen
  | StLBracket
  | StRBracket
  | StId of string
  | StQuote
  | StEOF

let str_s_token t = match t with
  | StLParen -> "("
  | StRParen -> ")"
  | StLBracket -> "["
  | StRBracket -> "]"
  | StId id -> "Id " ^ id
  | StQuote -> "'"
  | StEOF -> "EOF"
