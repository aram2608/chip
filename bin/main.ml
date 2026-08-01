open Chip

let parse_string (input : string) : Ast.stm =
  let lexbuf = Lexing.from_string input in
  try Parser.prog Lexer.token lexbuf
  with Parser.Error ->
    let pos = lexbuf.lex_start_p in
    failwith
      (Printf.sprintf "Parse error at line %d, column %d" pos.pos_lnum
         (pos.pos_cnum - pos.pos_bol))

let () =
  let count = Array.length Sys.argv in
  if count > 1 then begin
    let source = In_channel.with_open_text Sys.argv.(1) In_channel.input_all in
    let p = parse_string source in
    let proto = Chip.Compile.compile_program p in
    Chip.Vm.exec (Chip.Vm.make_vm proto)
  end
  else begin
    prerr_string "Missing file";
    ()
  end
