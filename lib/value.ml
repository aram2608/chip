open Bytecode

module Object = struct
  type value = Int of int | Float of float | String of string | Null
end

type block = { constants : Object.value array; code : int array }
type proto = { block : block; arity : int; max_regs : int }
