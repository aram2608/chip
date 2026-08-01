open Chip

let parse_string (input : string) : Ast.stm =
  let lexbuf = Lexing.from_string input in
  try Parser.prog Lexer.token lexbuf
  with Parser.Error ->
    let pos = lexbuf.lex_start_p in
    failwith
      (Printf.sprintf "Parse error at line %d, column %d" pos.pos_lnum
         (pos.pos_cnum - pos.pos_bol))

let run file dis =
  let source = In_channel.with_open_text file In_channel.input_all in
  let p = parse_string source in
  let proto = Chip.Compile.compile_program p in
  if dis then Chip.Dis.print_proto proto
  else Chip.Vm.exec (Chip.Vm.make_vm proto)

open Cmdliner

let file =
  let doc = "The chip source file to run." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let dis =
  let doc = "Disassemble the compiled bytecode instead of running it." in
  Arg.(value & flag & info [ "d"; "dis" ] ~doc)

let cmd =
  let doc = "Compile and run chip programs." in
  let info = Cmd.info "chip" ~doc in
  Cmd.v info Term.(const run $ file $ dis)

let () = exit (Cmd.eval cmd)
