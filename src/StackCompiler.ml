open Types

let rec compile_expression expression =
  match expression with
  | Var             x           -> [S_LD   x]
  | Const           n           -> [S_PUSH n]
  | UnaryOperation  (op, x)     -> compile_expression x @ [S_UNARY_OPERATION op]
  | BinaryOperation (op, l, r)  -> compile_expression l @ compile_expression r @ [S_BINARY_OPERATION op]
  | Comparison      (op, l, r)  -> compile_expression l @ compile_expression r @ [S_COMPARISON op]

let counter = 
  let count = ref(-1) in
  fun () -> incr count; !count

let rec compile_statement statement =
 match statement with
  | Skip              -> []
  | Assign (x, e)     -> compile_expression e @ [S_ST x]
  | Read    x         -> [S_READ; S_ST x]
  | Write   e         -> compile_expression e @ [S_WRITE]
  | Seq    (l, r)     -> compile_statement l @ compile_statement r
  | If     (e, t, f)  -> 
      let label_else  = counter() in
      let label_end   = counter() in
      compile_expression e @ [S_CJUMP ("Z", label_else)] @ compile_statement t @ [S_JUMP label_end; S_LABEL label_else] @ compile_statement f @ [S_LABEL label_end]
  | While  (e, t)     ->
      let label_code  = counter() in
      let label_cond  = counter() in
      [S_JUMP label_cond; S_LABEL label_code] @ compile_statement t @ [S_LABEL label_cond] @ compile_expression e @ [S_CJUMP ("NZ", label_code)]
