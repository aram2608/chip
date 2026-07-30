type value = VInt of int | VFlt of float | VStr of string

module SymbolMap = Map.Make (String)

type env = { table : value ref SymbolMap.t; parent : env option }

let empty_env = { table = SymbolMap.empty; parent = None }

let push_scope current_env =
  { table = SymbolMap.empty; parent = Some current_env }

let declare id value env =
  { env with table = SymbolMap.add id (ref value) env.table }

let rec lookup id env =
  match SymbolMap.find_opt id env.table with
  | Some v -> Some !v
  | None -> (
      match env.parent with
      | Some parent_env -> lookup id parent_env
      | None -> None)

(* try to assign and on failure return a false *)
let rec assign id value env =
  match SymbolMap.find_opt id env.table with
  | Some r ->
      r := value;
      true
  | None -> (
      match env.parent with Some p -> assign id value p | None -> false)

exception RuntimeError of string

let rec eval (env : env) = function
  | Ast.IntLit n -> VInt n
  | Ast.FloatLit n -> VFlt n
  | Ast.StringLit s -> VStr s
  | Ast.Var name -> (
      match lookup name env with
      | Some v -> v
      | None -> raise (RuntimeError ("Undefined variable: " ^ name)))
  | Ast.Binop (op, lhs, rhs) ->
      let v1 = eval env lhs in
      let v2 = eval env rhs in
      eval_binop op v1 v2

and eval_binop op v1 v2 =
  match (op, v1, v2) with
  (* Integer operations *)
  | Add, VInt x, VInt y -> VInt (x + y)
  | Sub, VInt x, VInt y -> VInt (x - y)
  | Mul, VInt x, VInt y -> VInt (x * y)
  | Div, VInt x, VInt y ->
      if y = 0 then raise (RuntimeError "Division by zero") else VInt (x / y)
  (* Float operations *)
  | Add, VFlt x, VFlt y -> VFlt (x +. y)
  | Sub, VFlt x, VFlt y -> VFlt (x -. y)
  | Mul, VFlt x, VFlt y -> VFlt (x *. y)
  | Div, VFlt x, VFlt y ->
      if y = 0.0 then raise (RuntimeError "Division by zero") else VFlt (x /. y)
  (* Mixed-mode operations (Coercing Int to Float) *)
  | Add, VInt x, VFlt y -> VFlt (float_of_int x +. y)
  | Add, VFlt x, VInt y -> VFlt (x +. float_of_int y)
  | Sub, VInt x, VFlt y -> VFlt (float_of_int x -. y)
  | Sub, VFlt x, VInt y -> VFlt (x -. float_of_int y)
  | Mul, VInt x, VFlt y -> VFlt (float_of_int x *. y)
  | Mul, VFlt x, VInt y -> VFlt (x *. float_of_int y)
  | Div, VInt x, VFlt y -> VFlt (float_of_int x /. y)
  | Div, VFlt x, VInt y -> VFlt (x /. float_of_int y)
  (* String Concatenation *)
  | Add, VStr x, VStr y -> VStr (x ^ y)
  | _ ->
      raise (RuntimeError "Type error: Incompatible types for binary operation")

let is_truthy v =
  match v with
  | VInt v -> v <> 0
  | VFlt v -> v <> 0.0
  | VStr v -> not (String.equal v "")

let rec interpret ?(env : env = empty_env) = function
  | Ast.Assign (x, e) ->
      let v = eval env e in
      if assign x v env then env else declare x v env
  | Ast.ExpStmt c ->
      ignore (eval env c);
      env
  | Ast.IfElse (c, t, e) ->
      interpret ~env (if is_truthy (eval env c) then t else e)
  | Ast.If (c, t) -> if is_truthy (eval env c) then interpret ~env t else env
  | Ast.While (c, b) as w ->
      if is_truthy (eval env c) then (
        ignore (interpret ~env b);
        interpret ~env w)
      else env
  | Ast.Print c ->
      (match eval env c with
      | VInt v -> print_endline (string_of_int v)
      | VFlt v -> print_endline (string_of_float v)
      | VStr v -> print_endline v);
      env
  | Ast.Block stmts ->
      let local_env = push_scope env in
      ignore
        (List.fold_left
           (fun acc_env stmt -> interpret ~env:acc_env stmt)
           local_env stmts);
      env
  | Ast.Seq stmts ->
      List.fold_left (fun acc_env stmt -> interpret ~env:acc_env stmt) env stmts
