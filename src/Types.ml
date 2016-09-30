module SS = Set.Make(String)

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

let rec collect_vars statement =
  let rec collect_vars_expression expression =
    match expression with
    | Const           _         -> SS.empty
    | Var             s         -> SS.singleton s
    | UnaryOperation  (_, x)    -> collect_vars_expression x 
    | BinaryOperation (_, l, r) -> SS.union (collect_vars_expression l) (collect_vars_expression r)
    | Comparison      (_, l, r) -> SS.union (collect_vars_expression l) (collect_vars_expression r)
  in
  match statement with
  | Skip            -> SS.empty
  | Seq     (l, r)  -> SS.union (collect_vars l) (collect_vars r)
  | Assign  (x, e)  -> SS.union (SS.singleton x) (collect_vars_expression e)
  | Write   e       -> collect_vars_expression e
  | Read    x       -> SS.singleton x

type stack_instruction = 
  | S_READ
  | S_WRITE
  | S_PUSH              of int
  | S_LD                of string
  | S_ST                of string
  | S_UNARY_OPERATION   of operation
  | S_BINARY_OPERATION  of operation
  | S_COMPARISON        of operation 

type x86instruction =
  | X86Ret
  | X86Div              of opnd
  | X86Mov              of opnd * opnd
  | X86Cmp              of opnd * opnd
  | X86Push             of opnd
  | X86Pop              of opnd
  | X86Call             of string
  | X86Label            of int
  | X86Jump             of operation * int
  | X86UnaryOperation   of operation * opnd
  | X86BinaryOperation  of operation * opnd * opnd

type opnd = R of int | S of int | M of string | L of int

