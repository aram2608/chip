type value = IntVal of int | FloatVal of float | StringVal of string
type env = (string, value) Hashtbl.t

exception RuntimeError of string

let rec eval (env : env) = function
  | Ast.IntLit n -> IntVal n
  | Ast.FloatLit n -> FloatVal n
  | Ast.StringLit s -> StringVal s
  | Ast.Var name -> (
      match Hashtbl.find_opt env name with
      | Some v -> v
      | None -> raise (RuntimeError ("Undefined variable: " ^ name)))
  | Ast.Binop (op, lhs, rhs) ->
      let v1 = eval env lhs in
      let v2 = eval env rhs in
      eval_binop op v1 v2

and eval_binop op v1 v2 =
  match (op, v1, v2) with
  (* Integer operations *)
  | Add, IntVal x, IntVal y -> IntVal (x + y)
  | Sub, IntVal x, IntVal y -> IntVal (x - y)
  | Mul, IntVal x, IntVal y -> IntVal (x * y)
  | Div, IntVal x, IntVal y ->
      if y = 0 then raise (RuntimeError "Division by zero") else IntVal (x / y)
  (* Float operations *)
  | Add, FloatVal x, FloatVal y -> FloatVal (x +. y)
  | Sub, FloatVal x, FloatVal y -> FloatVal (x -. y)
  | Mul, FloatVal x, FloatVal y -> FloatVal (x *. y)
  | Div, FloatVal x, FloatVal y ->
      if y = 0.0 then raise (RuntimeError "Division by zero")
      else FloatVal (x /. y)
  (* Mixed-mode operations (Coercing Int to Float) *)
  | Add, IntVal x, FloatVal y -> FloatVal (float_of_int x +. y)
  | Add, FloatVal x, IntVal y -> FloatVal (x +. float_of_int y)
  | Sub, IntVal x, FloatVal y -> FloatVal (float_of_int x -. y)
  | Sub, FloatVal x, IntVal y -> FloatVal (x -. float_of_int y)
  | Mul, IntVal x, FloatVal y -> FloatVal (float_of_int x *. y)
  | Mul, FloatVal x, IntVal y -> FloatVal (x *. float_of_int y)
  | Div, IntVal x, FloatVal y -> FloatVal (float_of_int x /. y)
  | Div, FloatVal x, IntVal y -> FloatVal (x /. float_of_int y)
  (* String Concatenation *)
  | Add, StringVal x, StringVal y -> StringVal (x ^ y)
  | _ ->
      raise (RuntimeError "Type error: Incompatible types for binary operation")

let is_truthy v =
  match v with
  | IntVal v -> v <> 0
  | FloatVal v -> v <> 0.0
  | StringVal v -> not (String.equal v "")

let rec interpret (env : env) = function
  | Ast.Assign (x, e) ->
      let v = eval env e in
      Hashtbl.replace env x v;
      env
  | Ast.ExpStmt c ->
      ignore (eval env c);
      env
  | Ast.IfElse (c, t, e) ->
      interpret env (if is_truthy (eval env c) then t else e)
  | Ast.If (c, t) -> if is_truthy (eval env c) then interpret env t else env
  | Ast.While (c, b) as w ->
      if is_truthy (eval env c) then
        let updated_env = interpret env b in
        interpret updated_env w
      else env
  | Ast.Print c ->
      (match eval env c with
      | IntVal v -> print_endline (string_of_int v)
      | FloatVal v -> print_endline (string_of_float v)
      | StringVal v -> print_endline v);
      env
  | Ast.Block stmts -> List.fold_left interpret env stmts
  | Ast.Seq stmts -> List.fold_left interpret env stmts
