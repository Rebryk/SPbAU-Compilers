open Types

let rec compile_expression expression =
  match expression with
  | Var             x           -> [S_LD   x]
  | Const           n           -> [S_PUSH n]
  | UnaryOperation  (op, x)     -> compile_expression x @ [S_UNARY_OPERATION op]
  | BinaryOperation (op, l, r)  -> compile_expression l @ compile_expression r @ [S_BINARY_OPERATION op]
  | Comparison      (op, l, r)  -> compile_expression l @ compile_expression r @ [S_COMPARISON op]

let rec compile_statement statement =
  match statement with
  | Skip          -> []
  | Assign (x, e) -> compile_expression e @ [S_ST x]
  | Read    x     -> [S_READ; S_ST x]
  | Write   e     -> compile_expression e @ [S_WRITE]
  | Seq    (l, r) -> compile_statement l @ compile_statement r
