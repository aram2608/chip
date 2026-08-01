open Bytecode
open Value

type expr_result =
  | KConst of Object.value
  | KIndex of int
  | Register of int
  | Local of int

let get_inst (b : block) (idx : int) : int = b.code.(idx)
let inst_count (b : block) : int = Array.length b.code

type builder = {
  constants : Object.value Dynarray.t;
  const_map : (Object.value, int) Hashtbl.t;
  code : int Dynarray.t;
}

let make_builder () =
  {
    constants = Dynarray.create ();
    const_map = Hashtbl.create 16;
    code = Dynarray.create ();
  }

let add_constant b v =
  match Hashtbl.find_opt b.const_map v with
  | Some idx -> idx
  | None ->
      let idx = Dynarray.length b.constants in
      Dynarray.add_last b.constants v;
      Hashtbl.add b.const_map v idx;
      idx

let get_constant builder idx = Dynarray.get builder.constants idx
let emit builder inst = Dynarray.add_last builder.code inst

(* Emit a placeholder jump and return its index so it can be patched later. *)
let emit_jmp builder =
  Dynarray.add_last builder.code (create_sj Jmp 0 0);
  Dynarray.length builder.code - 1

let patch_jmp_to_here builder idx =
  let offset = Dynarray.length builder.code - 1 - idx in
  Dynarray.set builder.code idx (create_sj Jmp offset 0)

let freeze builder : block =
  {
    constants = Dynarray.to_array builder.constants;
    code = Dynarray.to_array builder.code;
  }

type func_state = {
  b : builder;
  locals : (string, int) Hashtbl.t;
  mutable free_reg : int;
  mutable max_reg : int;
  mutable n_locals : int;
}

let make_fs () =
  {
    b = make_builder ();
    locals = Hashtbl.create 16;
    free_reg = 0;
    max_reg = 0;
    n_locals = 0;
  }

exception CompTimeError of string

let alloc_reg fs =
  let r = fs.free_reg in
  fs.free_reg <- fs.free_reg + 1;
  if fs.free_reg > fs.max_reg then fs.max_reg <- fs.free_reg;
  r

let free_reg fs r =
  if r >= fs.n_locals then begin
    assert (r = fs.free_reg - 1);
    fs.free_reg <- fs.free_reg - 1
  end

let free_exp fs = function Register r -> free_reg fs r | _ -> ()

let reserve_local fs name =
  match Hashtbl.find_opt fs.locals name with
  | Some slot -> slot
  | None ->
      let slot = fs.n_locals in
      Hashtbl.add fs.locals name slot;
      fs.n_locals <- fs.n_locals + 1;
      fs.free_reg <- fs.free_reg + 1;
      if fs.free_reg > fs.max_reg then fs.max_reg <- fs.free_reg;
      slot

let resolve_var fs name =
  match Hashtbl.find_opt fs.locals name with
  | Some slot -> Local slot
  | None -> raise (CompTimeError ("Undefined variable: " ^ name))

let reg_of = function Register r | Local r -> r | _ -> assert false

let discharge_any fs = function
  | (Register _ | Local _) as e -> e
  | KConst v ->
      let r = alloc_reg fs in
      emit fs.b (create_ABx LoadK r (add_constant fs.b v));
      Register r
  | KIndex i ->
      let r = alloc_reg fs in
      emit fs.b (create_ABx LoadK r i);
      Register r

let discharge_to_reg fs reg = function
  | KConst v -> emit fs.b (create_ABx LoadK reg (add_constant fs.b v))
  | KIndex i -> emit fs.b (create_ABx LoadK reg i)
  | Register r | Local r ->
      if r <> reg then emit fs.b (create_ABCk Move reg r 0 0)

let arith_of_binop = function
  | Ast.Add -> Add
  | Ast.Sub -> Sub
  | Ast.Mul -> Mult
  | Ast.Div -> Div

let rec compile_exp fs = function
  | Ast.Binop (op, lhs, rhs) -> (
      let e1 = compile_exp fs lhs in
      let e2 = compile_exp fs rhs in
      match (e1, e2) with
      | KConst (Object.Int a), KConst (Object.Int b) -> (
          match op with
          | Ast.Add -> KConst (Object.Int (a + b))
          | Ast.Sub -> KConst (Object.Int (a - b))
          | Ast.Mul -> KConst (Object.Int (a * b))
          | Ast.Div ->
              if b = 0 then raise (CompTimeError "Division by zero")
              else KConst (Object.Int (a / b)))
      | KConst (Object.Float a), KConst (Object.Float b) -> (
          match op with
          | Ast.Add -> KConst (Object.Float (a +. b))
          | Ast.Sub -> KConst (Object.Float (a -. b))
          | Ast.Mul -> KConst (Object.Float (a *. b))
          | Ast.Div ->
              if b = 0.0 then raise (CompTimeError "Division by zero")
              else KConst (Object.Float (a /. b)))
      | KConst (Object.String a), KConst (Object.String b) -> (
          match op with
          | Ast.Add -> KConst (Object.String (a ^ b))
          | _ -> raise (CompTimeError "Invalid operation for string literal"))
      | _ ->
          let e1 = discharge_any fs e1 in
          let e2 = discharge_any fs e2 in
          let a = reg_of e1 and b = reg_of e2 in
          free_exp fs e2;
          free_exp fs e1;
          let dst = alloc_reg fs in
          emit fs.b (create_ABCk (arith_of_binop op) dst a b 0);
          Register dst)
  | Ast.IntLit i -> KConst (Object.Int i)
  | Ast.FloatLit f -> KConst (Object.Float f)
  | Ast.StringLit s -> KConst (Object.String s)
  | Ast.Var name -> resolve_var fs name

let rec compile_stm fs = function
  | Ast.ExpStmt e ->
      let r = compile_exp fs e in
      free_exp fs r
  | Ast.Assign (x, e) ->
      let slot = reserve_local fs x in
      let e' = compile_exp fs e in
      discharge_to_reg fs slot e';
      free_exp fs e'
  | Ast.Print e ->
      let e' = compile_exp fs e in
      let r =
        match e' with
        | Register r -> r
        | _ ->
            let r = alloc_reg fs in
            discharge_to_reg fs r e';
            r
      in
      emit fs.b (create_ABCk Print r 0 0 0);
      free_exp fs (Register r)
  | Ast.Seq stmts | Ast.Block stmts -> List.iter (compile_stm fs) stmts
  | Ast.If (e, stm) ->
      let base = fs.free_reg in
      let c' = compile_exp fs e in
      let rc =
        match c' with
        | Register r -> r
        | _ ->
            let r = alloc_reg fs in
            discharge_to_reg fs r c';
            r
      in
      emit fs.b (create_ABCk Test rc 0 0 0);
      let jmp_end = emit_jmp fs.b in

      (* condition register is dead once Test/Jmp are emitted *)
      fs.free_reg <- base;
      compile_stm fs stm;
      patch_jmp_to_here fs.b jmp_end;
      (* body balances its own registers; restore to base defensively *)
      fs.free_reg <- base
  | _ -> raise (CompTimeError "TODO: CompileStmt")

let compile_program (s : Ast.stm) : proto =
  let fs = make_fs () in
  compile_stm fs s;
  { block = freeze fs.b; arity = 0; max_regs = fs.max_reg }
