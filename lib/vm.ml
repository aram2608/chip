open Bytecode
open Value
open Compile

type vm = {
  mutable regs : Object.value Array.t;
  mutable pc : int;
  mutable bp : int;
  proto : proto;
}

let make_vm p =
  { regs = Array.make (max_regs + 1) Object.Null; pc = 0; bp = 0; proto = p }

let r vm r = vm.regs.(vm.bp + r)

exception RuntimeError of string

let to_float = function
  | Object.Int i -> float_of_int i
  | Object.Float f -> f
  | Object.String _ -> raise (RuntimeError "expected number")
  | Object.Null -> raise (RuntimeError "expected number")

let apply (op : op) a b =
  let open Object in
  match (op, a, b) with
  | Add, Int x, Int y -> Int (x + y)
  | Sub, Int x, Int y -> Int (x - y)
  | Mult, Int x, Int y -> Int (x * y)
  | Div, Int x, Int y ->
      if y = 0 then raise (RuntimeError "Division by zero") else Int (x / y)
  | Add, String x, String y -> String (x ^ y)
  | (Add | Sub | Mult | Div), (Int _ | Float _), (Int _ | Float _) -> (
      let x = to_float a and y = to_float b in
      match op with
      | Add -> Float (x +. y)
      | Sub -> Float (x -. y)
      | Mult -> Float (x *. y)
      | Div ->
          if y = 0.0 then raise (RuntimeError "Division by zero")
          else Float (x /. y)
      | _ -> assert false)
  | _ -> raise (RuntimeError "Type error in binary operation")

let compare (op : op) a b =
  let open Object in
  match (op, a, b) with
  | LE, Int x, Int y -> Int (Bool.to_int (x <= y))
  | LT, Int x, Int y -> Int (Bool.to_int (x < y))
  | GE, Int x, Int y -> Int (Bool.to_int (x >= y))
  | GT, Int x, Int y -> Int (Bool.to_int (x > y))
  | NE, Int x, Int y -> Int (Bool.to_int (x <> y))
  | LE, Float x, Float y -> Int (Bool.to_int (x <= y))
  | LT, Float x, Float y -> Int (Bool.to_int (x < y))
  | GE, Float x, Float y -> Int (Bool.to_int (x >= y))
  | GT, Float x, Float y -> Int (Bool.to_int (x > y))
  | EQ, String x, String y -> Int (Bool.to_int (String.equal x y))
  | NE, String x, String y -> Int (Bool.to_int (x <> y))
  | _ -> raise (RuntimeError "Type error in comparison operation")

let print_value = function
  | Object.Int i -> print_endline (string_of_int i)
  | Object.Float f -> print_endline (string_of_float f)
  | Object.String s -> print_endline s
  | Object.Null -> print_endline "null"

let is_truthy = function
  | Object.Int i -> i <> 0
  | Object.Float f -> f <> 0.0
  | Object.String s -> s <> ""
  | Object.Null -> false

let set vm i v = vm.regs.(vm.bp + i) <- v

let exec (vm : vm) =
  let consts = vm.proto.block.constants in
  let code = vm.proto.block.code in
  let n = Array.length code in
  while vm.pc < n do
    let i = code.(vm.pc) in
    (match op_of_enum (get_opcode i) with
    | Some LoadK -> set vm (get_a i) consts.(get_bx i)
    | Some Move -> set vm (get_a i) (r vm (get_b i))
    | Some ((Add | Sub | Mult | Div) as op) ->
        set vm (get_a i) (apply op (r vm (get_b i)) (r vm (get_c i)))
    | Some ((LE | LT | GE | GT | NE | EQ) as op) ->
        set vm (get_a i) (compare op (r vm (get_b i)) (r vm (get_c i)))
    | Some Print -> print_value (r vm (get_a i))
    | Some Test ->
        if is_truthy vm.regs.(get_a i) <> (get_k i = 1) then vm.pc <- vm.pc + 1
    | Some Jmp -> vm.pc <- vm.pc + get_sj i
    | Some _ | None -> raise (RuntimeError "Unknown opcode"));
    vm.pc <- vm.pc + 1
  done
