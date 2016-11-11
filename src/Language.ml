module Expr = struct
  type operation =
  | Not
  | Add 
  | Sub 
  | Mul
  | Div
  | Mod 
  | And 
  | Or 
  | Less 
  | Leq
  | Equal
  | Geq
  | Greater 
  | Neq

type expression = 
  | Const             of int
  | Var               of string
  | UnaryOperation    of operation * expression
  | BinaryOperation   of operation * expression * expression
  | Comparison        of operation * expression * expression

type statement =
  | Skip
  | Read   of string
  | Write  of expression
  | Assign of string * expression
  | Seq    of statement * statement
  | If     of expression * statement * statement
  | While  of expression * statement

end
