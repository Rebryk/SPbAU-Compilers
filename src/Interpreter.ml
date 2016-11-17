module Interpeter = 
  struct
  
  open Language.Expr

  let rec evaluate state expression =
  let to_bool x = x != 0 in
  let to_int  x = if x then 1 else 0 in

  match expression with
  | Const           n               -> n
  | Var             x               -> state x
  | UnaryOperation  (Not,     e)    -> to_int (not (to_bool (evaluate state e)))
  | BinaryOperation (Add,     l, r) -> evaluate state l + evaluate state r
  | BinaryOperation (Sub,     l, r) -> evaluate state l - evaluate state r
  | BinaryOperation (Mul,     l, r) -> evaluate state l * evaluate state r
  | BinaryOperation (Div,     l, r) -> evaluate state l / evaluate state r
  | BinaryOperation (Mod,     l, r) -> evaluate state l mod evaluate state r
  | BinaryOperation (And,     l, r) -> to_int (to_bool (evaluate state l) && to_bool (evaluate state r))
  | BinaryOperation (Or,      l, r) -> to_int (to_bool (evaluate state l) || to_bool (evaluate state r))
  | Comparison      (Less,    l, r) -> to_int (evaluate state l < evaluate state r)
  | Comparison      (Leq,     l, r) -> to_int (evaluate state l <= evaluate state r)
  | Comparison      (Equal,   l, r) -> to_int (evaluate state l == evaluate state r)
  | Comparison      (Geq,     l, r) -> to_int (evaluate state l >= evaluate state r)
  | Comparison      (Greater, l, r) -> to_int (evaluate state l > evaluate state r)
  | Comparison      (Neq,     l, r) -> to_int (evaluate state l != evaluate state r)
  | _                               -> failwith "Unexpected expression type"


let run input statement =
  let rec run' ((state, input, output) as c) statement =
    let state' x = List.assoc x state in
    match statement with
    | Skip              -> c
    | Seq     (l, r)    -> run' (run' c l) r
    | Assign  (x, e)    -> ((x, evaluate state' e) :: state, input, output)
    | Write   e         -> (state, input, output @ [evaluate state' e])
    | If      (e, l, r) -> 
      let value = evaluate state' e in
      let result = 
        match value with
        | 0 -> run' c r
        | _ -> run' c l
      in result
    | While   (e, m)    ->
      let value = evaluate state' e in
      let result = 
        match value with
        | 0 -> c
        | _ -> run' (run' c m) statement
      in result
    | Read    x         ->
      let result = 
        match input with
        | []        -> failwith "Unexpected end of stack"
        | y::input' -> ((x, y) :: state, input', output)
       in result
  in
  let (_, _, result) = run' ([], input, []) statement in
  result  
end