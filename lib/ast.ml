type binop = Add | Sub | Div | Mul

type expr =
  | Var of string
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | Binop of binop * expr * expr

type stm =
  | Assign of string * expr
  | While of expr * stm
  | Block of stm list
  | Seq of stm list
  | If of expr * stm
  | IfElse of expr * stm * stm
  | ExpStmt of expr
  | Print of expr
