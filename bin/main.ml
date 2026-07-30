open Chip

let parse_string (input : string) : Ast.stm =
  let lexbuf = Lexing.from_string input in
  try Parser.prog Lexer.token lexbuf
  with Parser.Error ->
    let pos = lexbuf.lex_start_p in
    failwith
      (Printf.sprintf "Parse error at line %d, column %d" pos.pos_lnum
         (pos.pos_cnum - pos.pos_bol))

let p = parse_string "a := 4.0; print a; b := 2; print a / b"
