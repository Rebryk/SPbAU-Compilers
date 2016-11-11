open Ostap 
open Matcher

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

type stack_instruction = 
  | S_READ
  | S_WRITE
  | S_PUSH              of int
  | S_LD                of string
  | S_ST                of string
  | S_UNARY_OPERATION   of operation
  | S_BINARY_OPERATION  of operation
  | S_COMPARISON        of operation 
  | S_LABEL             of int
  | S_JUMP              of int
  | S_CJUMP             of string * int

type opnd = S_R of int | R of int | S of int | M of string | L of int

type x86instruction =
  | X86Ret
  | X86Set              of operation * opnd
  | X86Div              of opnd
  | X86Mov              of opnd * opnd
  | X86Cmp              of opnd * opnd
  | X86Push             of opnd
  | X86Pop              of opnd
  | X86Call             of string
  | X86Label            of int
  | X86Jump             of string * int
  | X86UnaryOperation   of operation * opnd
  | X86BinaryOperation  of operation * opnd * opnd

