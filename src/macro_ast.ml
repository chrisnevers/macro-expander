open List

(*
  expr :=
    | (lambda (<id>) <expr>)      function
    | <id>                        var
    | (<expr> <expr> ...)         function call
    | (quote <datum>)             literal data
    | (let-syntax ([<id> <expr>]) macro binding
        <expr>)
    | (quote-syntax <datum>)      literal syntax
 *)
type s_exp =
  | SLambda of string * s_exp
  | SId of string
  | SApply of s_exp * s_exp list
  | SQuote of s_datum
  | SLetSyntax of string * s_exp * s_exp
  | SQuoteSyntax of s_datum

and s_datum =
  | SDatum of s_exp

let rec str_s_exp e = match e with
  | SLambda (id, e) -> "(LAMBDA (" ^ id ^ ") " ^ str_s_exp e ^ ")"
  | SId id -> id
  | SApply (id, args) -> "(" ^ str_s_exp id ^ " " ^ String.concat " " (map str_s_exp args) ^ ")"
  | SQuote d -> "'" ^ str_s_datum d
  | SLetSyntax (id, p, b) -> "(LET-SYNTAX ([" ^ id ^ " " ^ str_s_exp p ^ "]) "
                              ^ str_s_exp b ^ ")"
  | SQuoteSyntax d -> "(QUOTE-SYNTAX " ^ str_s_datum d ^ ")"

and str_s_datum a = match a with
  | SDatum s -> str_s_exp s
