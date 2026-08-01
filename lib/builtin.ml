open Value

type builtin = {
  name : string;
  arity : int;
  f : Object.value array -> Object.value;
}

let repr = function
  | Object.Float f -> string_of_float f
  | Object.Int i -> string_of_int i
  | Object.String s -> s
  | Object.Null -> "null"

let bprint args =
  Array.iter (fun x -> print_string (repr x)) args;
  print_newline ();
  Object.Null

let natives = [| { name = "@print"; arity = Int.max_int; f = bprint } |]
