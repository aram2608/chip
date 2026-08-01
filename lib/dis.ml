open Bytecode
open Value

let string_of_value = function
  | Object.Int i -> string_of_int i
  | Object.Float f -> string_of_float f
  | Object.String s -> Printf.sprintf "%S" s
  | Object.Null -> "null"

let string_of_inst consts pc i =
  match op_of_enum (get_opcode i) with
  | None -> Printf.sprintf "<bad opcode %d>" (get_opcode i)
  | Some op ->
      let a = get_a i and b = get_b i and c = get_c i in
      let k = get_k i in
      let short =
        let s = show_op op in
        match String.rindex_opt s '.' with
        | Some i -> String.sub s (i + 1) (String.length s - i - 1)
        | None -> s
      in
      let name = Printf.sprintf "%-6s" short in
      let operands, comment =
        match op with
        | Move -> (Printf.sprintf "r%d r%d" a b, None)
        | LoadK ->
            let bx = get_bx i in
            let v =
              if bx < Array.length consts then string_of_value consts.(bx)
              else "?"
            in
            (Printf.sprintf "r%d K%d" a bx, Some v)
        | Add | Sub | Mult | Div | LT | LE | GT | GE | EQ | NE | BAnd | BOr
        | BXor | BNot ->
            (Printf.sprintf "r%d r%d r%d" a b c, None)
        | Test -> (Printf.sprintf "r%d (k=%d)" a k, None)
        | Jmp -> (Printf.sprintf "-> %d" (pc + get_sj i + 1), None)
        | CallB ->
            let nm =
              if c < Array.length Builtin.natives then Builtin.natives.(c).name
              else Printf.sprintf "#%d" c
            in
            (Printf.sprintf "base=r%d argc=%d" a b, Some nm)
        | Call -> (Printf.sprintf "base=r%d argc=%d ret=%d" a b c, None)
        | GGet | GSet -> (Printf.sprintf "r%d K%d" a (get_bx i), None)
        | Print -> (Printf.sprintf "r%d" a, None)
        | Ret -> ("", None)
      in
      let line = Printf.sprintf "%s %s" name operands in
      match comment with
      | None -> line
      | Some c -> Printf.sprintf "%-28s; %s" line c

let disassemble (p : proto) : string =
  let buf = Buffer.create 256 in
  let consts = p.block.constants in
  Buffer.add_string buf
    (Printf.sprintf "; proto  arity=%d  max_regs=%d\n" p.arity p.max_regs);
  Array.iteri
    (fun idx v ->
      Buffer.add_string buf (Printf.sprintf "; K%d = %s\n" idx (string_of_value v)))
    consts;
  Buffer.add_char buf '\n';
  Array.iteri
    (fun pc i ->
      Buffer.add_string buf
        (Printf.sprintf "%4d  %s\n" pc (string_of_inst consts pc i)))
    p.block.code;
  Buffer.contents buf

let print_proto (p : proto) = print_string (disassemble p)
