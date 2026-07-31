module Code = struct
  type rk = R of int | K of int

  type inst =
    | LoadK of int * int
    | Move of int * int
    | Add of int * rk * rk
    | Sub of int * rk * rk
    | Mul of int * rk * rk
    | Div of int * rk * rk
    | Print of int
end
