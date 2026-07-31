open Bytecode
open Value

exception RuntimeError of string

let to_float = function
  | Object.Int i -> float_of_int i
  | Object.Float f -> f
  | Object.String _ -> raise (RuntimeError "expected number")

let apply op a b =
  let open Object in
  match (op, a, b) with
  | `Add, Int x, Int y -> Int (x + y)
  | `Sub, Int x, Int y -> Int (x - y)
  | `Mul, Int x, Int y -> Int (x * y)
  | `Div, Int x, Int y ->
      if y = 0 then raise (RuntimeError "Division by zero") else Int (x / y)
  | `Add, String x, String y -> String (x ^ y)
  | (`Add | `Sub | `Mul | `Div), (Int _ | Float _), (Int _ | Float _) -> (
      let x = to_float a and y = to_float b in
      match op with
      | `Add -> Float (x +. y)
      | `Sub -> Float (x -. y)
      | `Mul -> Float (x *. y)
      | `Div ->
          if y = 0.0 then raise (RuntimeError "Division by zero")
          else Float (x /. y))
  | _ -> raise (RuntimeError "Type error in binary operation")

let print_value = function
  | Object.Int i -> print_endline (string_of_int i)
  | Object.Float f -> print_endline (string_of_float f)
  | Object.String s -> print_endline s

let exec (p : proto) =
  let consts = p.block.constants in
  let code = p.block.code in
  let n = Array.length code in
  let regs = Array.make (max p.max_regs 1) (Object.Int 0) in
  let rk = function Code.R r -> regs.(r) | Code.K k -> consts.(k) in
  let pc = ref 0 in
  while !pc < n do
    (match code.(!pc) with
    | Code.LoadK (dst, k) -> regs.(dst) <- consts.(k)
    | Code.Move (dst, src) -> regs.(dst) <- regs.(src)
    | Code.Add (dst, a, b) -> regs.(dst) <- apply `Add (rk a) (rk b)
    | Code.Sub (dst, a, b) -> regs.(dst) <- apply `Sub (rk a) (rk b)
    | Code.Mul (dst, a, b) -> regs.(dst) <- apply `Mul (rk a) (rk b)
    | Code.Div (dst, a, b) -> regs.(dst) <- apply `Div (rk a) (rk b)
    | Code.Print r -> print_value regs.(r));
    incr pc
  done
