module SS = Set.Make(String)

type operation =
  | Not     of expr
  | Add     of expr * expr
  | Sub     of expr * expr
  | Mul     of expr * expr
  | Div     of expr * expr
  | Mod     of expr * expr
  | And     of expr * expr
  | Or      of expr * expr
  | Less    of expr * expr
  | Leq     of expr * expr
  | Equal   of expr * expr
  | Geq     of expr * expr
  | Greater of expr * expr
  | Neq     of expr * expr 

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

let rec evaluate state expression =
  let to_bool x = x != 0 in
  let to_int  x = if x then 1 else 0 in

  match expression with
  | Const                   n       -> n
  | Var                     x       -> state x
  | UnaryOperation  Not     e       -> to_int (not (to_bool (eval state e)))
  | BinaryOperation Add     (l, r)  -> eval state l + eval state r
  | BinaryOperation Sub     (l, r)  -> eval state l - eval state r
  | BinaryOperation Mul     (l, r)  -> eval state l * eval state r
  | BinaryOperation Div     (l, r)  -> eval state l / eval state r
  | BinaryOperation Mod     (l, r)  -> eval state l mod eval state r
  | BinaryOperation And     (l, r)  -> to_int (to_bool (eval state l) && to_bool (eval state r))
  | BinaryOperation Or      (l, r)  -> to_int (to_bool (eval state l) || to_bool (eval state r))
  | Comparison      Less    (l, r)  -> to_int (eval state l < eval state r)
  | Comparison      Leq     (l, r)  -> to_int (eval state l <= eval state r)
  | Comparison      Equal   (l, r)  -> to_int (eval state l == eval state r)
  | Comparison      Geq     (l, r)  -> to_int (eval state l >= eval state r)
  | Comparison      Greater (l, r)  -> to_int (eval state l > eval state r)
  | Comparison      Neq     (l, r)  -> to_int (eval state l != eval state r)

let run input statement =
  let rec run' ((state, input, output) as c) statement =
    let state' x = List.assoc x state in
    match statement with
    | Skip            -> c
    | Seq     (l, r)  -> run' (run' c l) r
    | Assign  (x, e)  -> ((x, eval state' e) :: state, input, output)
    | Write   e       -> (state, input, output @ [eval state' e])
    | Read    x       ->
      let y::input' = input in (* TODO: check input *)
      ((x, y) :: state, input', output)
  in
  let (_, _, result) = run' ([], input, []) statement in
  result

let rec collect_vars statement =
  let rec collect_vars_expression expression =
    match expression with
    | Const           _       -> SS.empty
    | Var             s       -> SS.singleton s
    | UnaryOperation  _ x     -> collect_vars_expression x 
    | BinaryOperation _ l r   -> SS.union (collect_vars_expression l) (collect_vars_expression r)
    | Comparison      _ l r   -> SS.union (collect_vars_expression l) (collect_vars_expression r)
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

let stack_run input code =
  let rec stack_run' (state, stack, input, output) code =
    let to_bool x   = x != 0 in
    let to_int  x   = if x then 1 else 0 in

    let run_unary_operation operation = 
      let y::stack' = 
        match stack with
        | []        -> assert false
        | y::stack' -> stack
      in

      let result = 
        match operation with
        | Not   -> to_int (not (to_bool y))
        | _       -> assert false
      in
      (state, result::stack', input, output)
    in

    let run_binary_operation operation =
      let y::x::stack' = 
        match stack with
        | [] | _::[]    -> assert false
        | y::x::stack'  -> stack
      in

      let result = 
        match operation with
        | Add   -> x + y
        | Sub   -> x - y
        | Mul   -> x * y
        | Div   -> x / y
        | Mod   -> x mod y
        | And   -> to_int (to_bool x && to_bool y)
        | Or    -> to_int (to_bool x || to_bool y)
        | _     -> assert false
      in
      (state, result::stack', input, output)
    in

    let run_comparison stack_operation =
      let y::x::stack' = 
        match stack with
        | [] | _::[]    -> assert false
        | y::x::stack'  -> stack
      in

      let result = 
        match stack_operation with
        | Less      -> to_int (x < y) 
        | Leq       -> to_int (x <= y) 
        | Equal     -> to_int (x == y) 
        | Geq       -> to_int (x >= y) 
        | Greater   -> to_int (x > y) 
        | Neq       -> to_int (x != y) 
        | _         -> assert false
      in
      (state, result::stack', input, output)
    in

    match code with
    | []       -> output
    | i::code' ->
       stack_run'
         (match i with
          | S_READ    ->
              let y::input' = input in
              (state, y::stack, input', output)
          | S_WRITE   ->
              let y::stack' = stack in
              (state, stack', input, output @ [y])
          | S_PUSH n  ->
              (state, n::stack, input, output)
          | S_LD x    ->
              (state, (List.assoc x state)::stack, input, output)
          | S_ST x    ->
              let y::stack' = stack in
              ((x, y)::state, stack', input, output)
          | S_UNARY_OPERATION   op  -> run_unary_operation op
          | S_BINARY_OPERATION  op  -> run_binary_operation op
          | S_COMPARISON        op  -> run_comparison op
          | _                       -> assert false 
         )
         code'
  in
  stack_run' ([], [], input, []) code

let rec compile_expression expression =
  match expression with
  | Var             x       -> [S_LD   x]
  | Const           n       -> [S_PUSH n]
  | UnaryOperation  op x    -> compile_expression x @ [S_UNARY_OPERATION op]
  | BinaryOperation op l r  -> compile_expression l @ compile_expression r @ [S_BINARY_OPERATION op]
  | Comparison      op l r  -> compile_expression l @ compile_expression r @ [S_COMPARISON op]

let rec compile_statement statement =
  match statement with
  | Skip          -> []
  | Assign (x, e) -> compile_expression e @ [S_ST x]
  | Read    x     -> [S_READ; S_ST x]
  | Write   e     -> compile_expression e @ [S_WRITE]
  | Seq    (l, r) -> compile_statement l @ compile_statement r

let x86regs = [|"%esp"; "%ebp"; "%eax"; "%edx"; "%ecx"; "%ebx"; "%esi"; "%edi"|]
let num_of_regs = Array.length x86regs
let word_size = 4

type opnd = R of int | S of int | M of string | L of int

let x86esp = R 0
let x86ebp = R 1
let x86eax = R 2
let x86edx = R 3

let allocate stack =
  match stack with
  | []                              -> R 4 (* preserve esp, ebp, eax and edx for stuff :) *)
  | (S n)::_                        -> S (n+1)
  | (R n)::_ when n < num_of_regs-1 -> R (n+1)
  | _                               -> S 0

type x86instruction =
  | X86Ret
  | X86Div
  | X86Mov              of opnd * opnd
  | X86Cmp              of opnd * opnd
  | X86Push             of opnd
  | X86Pop              of opnd
  | X86Call             of string
  | X86Label            of int
  | X86Jump             of operation * int
  | X86UnaryOperation   of operation * opnd
  | X86BinaryOperation  of operation * opnd * opnd

let x86compile : instruction list -> x86instruction list = fun code ->
  let x86addStack s =
    match s with
    | S _ -> [X86BinaryOperation (Sub, L word_size, x86esp)]
    | _   -> []
  in
  
  let x86subStack s =
    match s with
    | S _ -> [X86BinaryOperation (Add, L word_size, x86esp)]
    | _   -> []
  in

  let check_stack stack   =
    match stack with
    | []        -> assert false
    | x::stack' -> stack
  in

  let check_stack_2 stack = 
    match stack with
    | [] | _::[]    -> assert false
    | y::x::stack'  -> stack
  in

  let rec x86compile' stack code label =
    let compile_comparison operation stack code label =
      let y::x::stack' = сheck_stack_2 stack in
      let result =   
        match x with
        | R _ -> (x::stack', [X86Cmp (y, x); X86Mov (L 1, x); X86Jump (operation, label); X86Mov (L 0, x); X86Label label] @ x86subStack y, label)
        | _   -> (x::stack', [X86Mov (x, x86eax); X86Cmp (y, x86eax); X86Mov (L 1, x86eax); X86Jump (operation, label); X86Mov (L 0, x86eax); X86Label label] @ x86subStack y, label)
      in 
      result 
    in

    let compile_binary_operation operation stack code label = 
      let y::x::stack' = сheck_stack_2 stack in
      let result = 
        match x with
        | R _ -> (x::stack', [X86BinaryOperation operation y x] @ x86subStack y, label)
        | _   -> (x::stack', [X86Mov (x, x86eax); X86BinaryOperation operation y x86eax; X86Mov (x86eax, x)] @ x86subStack y, label)
      in 
      result

    let compile_unary_operation operation stack code label =
      let x::stack' = check_stack stack in
      (stack, [X86UnaryOperation (operation, x)], label)
    in

    match code with
    | []       -> []
    | i::code' ->
      let (stack', x86code, label') =
        match i with
        | S_READ     ->
          let s = allocate stack in
          (s::stack, [X86Call "read"; X86Mov (x86eax, s)] @ x86addStack s, label)
        | S_WRITE    ->
          let s::stack' = check_stack stack in
          (stack', [X86Push s; X86Call "write"] @ x86subStack (S 0) @ x86subStack s, label)
        | S_PUSH n   ->
          let s = allocate stack in
          (s::stack, [X86Mov (L n, s)] @ x86addStack s, label)
        | S_LD x     ->
          let s = allocate stack in
          let result = 
            match s with
            | R _ -> (s::stack, [X86Mov (M x, s)], label)
            | _   -> (s::stack, [X86Mov (M x, x86eax); X86Mov (x86eax, s); X86BinaryOperation (Sub, L word_size, x86esp)], label)
				  in result
        | S_ST x     ->
          let s::stack' = check_stack stack in
          let result = 
            match s with
            | R _ -> (stack', [X86Mov (s, M x)], label)
            | _   -> (stack', [X86Mov (s, x86eax); X86Mov(x86eax, M x); X86BinaryOperation (Add, L word_size, x86esp)], label)
          in result
        | S_UNARY_OPERATION op    -> compile_unary_operation op stack code label
        | S_BINARY_OPERATION Div  ->
          let y::x::stack' = сheck_stack_2 stack in
          (x::stack', [X86Mov (x, x86eax); X86Div y; X86Mov (x86eax, x)], label)
        | S_BINARY_OPERATION Mod  ->
          let y::x::stack' = сheck_stack_2 stack in
          (x::stack', [X86Mov (x, x86eax); X86Div y; X86Mov (x86edx, x)], label)
        | S_BINARY_OPERATION  op  -> compile_binary_operation op stack code label
        | S_COMPARISON        op  -> compile_comparison op stack code (label + 1)
       in
       x86code @ x86compile' stack' code' label'
  in
  (X86Mov (x86esp, x86ebp))::x86compile' [] code 0

