open Bytecode

module Object = struct
  type value = Int of int | Float of float | String of string
end

type block = { constants : Object.value array; code : Code.inst array }
type proto = { block : block; arity : int; max_regs : int }
