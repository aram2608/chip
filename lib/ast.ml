type binop = Add | Sub | Div | Mul
type comp = Lesser | Greater | LEqual | GEqual | Equal | NEqual

type expr =
  | Var of string
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | Binop of binop * expr * expr
  | Comp of comp * expr * expr

type stm =
  | BuiltinCall of string * expr list
  | Decl of string * expr
  | Assign of string * expr
  | While of expr * stm
  | For of stm * expr * stm * stm
  | Return of expr
  | Block of stm list
  | Seq of stm list
  | If of expr * stm
  | IfElse of expr * stm * stm
  | ExpStmt of expr
