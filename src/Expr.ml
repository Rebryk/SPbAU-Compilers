module SS = Set.Make(String)

type expr =
  | Const   of int
  | Var     of string
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


let rec eval state expr =
  let to_bool x = x != 0 in
  let to_int  x = if x then 1 else 0 in

  match expr with
  | Const   n       -> n
  | Var     x       -> state x
  | Not     x       -> to_int (not (to_bool (eval state x)))
  | Add     (l, r)  -> eval state l + eval state r
  | Sub     (l, r)  -> eval state l - eval state r
  | Mul     (l, r)  -> eval state l * eval state r
  | Div     (l, r)  -> eval state l / eval state r
  | Mod     (l, r)  -> eval state l mod eval state r
  | And     (l, r)  -> to_int (to_bool (eval state l) && to_bool (eval state r))
  | Or      (l, r)  -> to_int (to_bool (eval state l) || to_bool (eval state r))
  | Less    (l, r)  -> to_int (eval state l < eval state r)
  | Leq     (l, r)  -> to_int (eval state l <= eval state r)
  | Equal   (l, r)  -> to_int (eval state l == eval state r)
  | Geq     (l, r)  -> to_int (eval state l >= eval state r)
  | Greater (l, r)  -> to_int (eval state l > eval state r)
  | Neq     (l, r)  -> to_int (eval state l != eval state r)

type stmt =
  | Skip
  | Read   of string
  | Write  of expr
  | Assign of string * expr
  | Seq    of stmt * stmt

let run input stmt =
  let rec run' ((state, input, output) as c) stmt =
    let state' x = List.assoc x state in
    match stmt with
    | Skip          -> c
    | Seq    (l, r) -> run' (run' c l) r
    | Assign (x, e) -> ((x, eval state' e) :: state, input, output)
    | Write   e     -> (state, input, output @ [eval state' e])
    | Read    x     ->
       let y::input' = input in
       ((x, y) :: state, input', output)
  in
  let (_, _, result) = run' ([], input, []) stmt in
  result

let rec collect_vars stmt =
  let rec collect_vars_expr expr =
    match expr with
    | Const    _      -> SS.empty
    | Var     s       -> SS.singleton s
    | Not     x       -> collect_vars_expr x 
    | Add     (l, r)  -> SS.union (collect_vars_expr l) (collect_vars_expr r)
    | Sub     (l, r)  -> SS.union (collect_vars_expr l) (collect_vars_expr r)
    | Mul     (l, r)  -> SS.union (collect_vars_expr l) (collect_vars_expr r)
    | Div     (l, r)  -> SS.union (collect_vars_expr l) (collect_vars_expr r)
    | Mod     (l, r)  -> SS.union (collect_vars_expr l) (collect_vars_expr r)
    | And     (l, r)  -> SS.union (collect_vars_expr l) (collect_vars_expr r)
    | Or      (l, r)  -> SS.union (collect_vars_expr l) (collect_vars_expr r)
    | Less    (l, r)  -> SS.union (collect_vars_expr l) (collect_vars_expr r)
    | Leq     (l, r)  -> SS.union (collect_vars_expr l) (collect_vars_expr r)
    | Equal   (l, r)  -> SS.union (collect_vars_expr l) (collect_vars_expr r)
    | Geq     (l, r)  -> SS.union (collect_vars_expr l) (collect_vars_expr r)
    | Greater (l, r)  -> SS.union (collect_vars_expr l) (collect_vars_expr r)
    | Neq     (l, r)  -> SS.union (collect_vars_expr l) (collect_vars_expr r)
  in
  match stmt with
  | Skip -> SS.empty
  | Seq (l, r) -> SS.union (collect_vars l) (collect_vars r)
  | Assign (x, e) -> SS.union (SS.singleton x) (collect_vars_expr e)
  | Write e -> collect_vars_expr e
  | Read x -> SS.singleton x

type instr =
  | S_READ
  | S_WRITE
  | S_PUSH  of int
  | S_LD    of string
  | S_ST    of string
  | S_NOT
  | S_ADD
  | S_SUB
  | S_MUL
  | S_DIV
  | S_MOD
  | S_AND
  | S_OR
  | S_LESS
  | S_LEQ
  | S_EQUAL
  | S_GEQ
  | S_GREATER
  | S_NEQ
  

let srun input code =
  let rec srun' (state, stack, input, output) code =
    let to_bool x = x != 0 in
    let to_int  x = if x then 1 else 0 in

    match code with
    | []       -> output
    | i::code' ->
       srun'
         (match i with
          | S_READ  ->
              let y::input' = input in
              (state, y::stack, input', output)
          | S_WRITE ->
              let y::stack' = stack in
              (state, stack', input, output @ [y])
          | S_PUSH n ->
              (state, n::stack, input, output)
          | S_LD x  ->
              (state, (List.assoc x state)::stack, input, output)
          | S_ST x  ->
              let y::stack' = stack in
              ((x, y)::state, stack', input, output)
          | S_NOT   ->
              let y::stack' = stack in
              (state, (to_int (not (to_bool y)))::stack', input, output)
          | S_ADD   ->
              let y::x::stack' = stack in
              (state, (x+y)::stack', input, output)
          | S_SUB   ->
              let y::x::stack' = stack in
              (state, (x - y)::stack', input, output)
          | S_MUL   ->
              let y::x::stack' = stack in
              (state, (x * y)::stack', input, output)
          | S_DIV   ->
              let y::x::stack' = stack in
              (state, (x / y)::stack', input, output)
          | S_MOD   ->
              let y::x::stack' = stack in
              (state, (x mod y)::stack', input, output)
         )
         code'
  in
  srun' ([], [], input, []) code

let rec compile_expr expr =
  match expr with
  | Var    x     -> [S_LD   x]
  | Const  n     -> [S_PUSH n]
  | Add   (l, r) -> compile_expr l @ compile_expr r @ [S_ADD]
  | Sub   (l, r) -> compile_expr l @ compile_expr r @ [S_SUB]
  | Mul   (l, r) -> compile_expr l @ compile_expr r @ [S_MUL]
  | Div   (l, r) -> compile_expr l @ compile_expr r @ [S_DIV]
  | Mod   (l, r) -> compile_expr l @ compile_expr r @ [S_MOD]

let rec compile_stmt stmt =
  match stmt with
  | Skip          -> []
  | Assign (x, e) -> compile_expr e @ [S_ST x]
  | Read    x     -> [S_READ; S_ST x]
  | Write   e     -> compile_expr e @ [S_WRITE]
  | Seq    (l, r) -> compile_stmt l @ compile_stmt r

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
  | []                              -> R 4 (* preserve esp, ebp and eax and for stuff :) *)
  | (S n)::_                        -> S (n+1)
  | (R n)::_ when n < num_of_regs-1 -> R (n+1)
  | _                               -> S 0

type x86instr = (* src -> dest *)
  | X86Add  of opnd * opnd
  | X86Sub  of opnd * opnd
  | X86Mul  of opnd * opnd
  | X86Div  of opnd
  | X86Mov  of opnd * opnd
  | X86Push of opnd
  | X86Pop  of opnd
  | X86Ret
  | X86Call of string

let x86compile : instr list -> x86instr list = fun code ->
  let x86addStack s =
    match s with
    | S _ -> [X86Sub (L word_size, x86esp)]
    | _   -> []
  in
  let x86subStack s =
    match s with
    | S _ -> [X86Add (L word_size, x86esp)]
    | _   -> []
  in
  let rec x86compile' stack code =
    match code with
    | []       -> []
    | i::code' ->
       let (stack', x86code) =
         match i with
         | S_READ   ->
           let s = allocate stack in
           (s::stack, [X86Call "read"; X86Mov (x86eax, s)] @ x86addStack s)
         | S_WRITE  ->
           let s::stack' = stack in
           (stack', [X86Push s; X86Call "write"] @ x86subStack (S 0) @ x86subStack s)
         | S_PUSH n ->
           let s = allocate stack in
           (s::stack, [X86Mov (L n, s)] @ x86addStack s)
         | S_LD x ->
           let s = allocate stack in
					 let res = match s with
           | R _ -> (s::stack, [X86Mov (M x, s)])
					 | _   -> (s::stack, [X86Mov (M x, x86eax); X86Mov (x86eax, s); X86Sub (L word_size, x86esp)])
					 in res
         | S_ST x ->
           let s::stack' = stack in
					 let res = match s with
           | R _ -> (stack', [X86Mov (s, M x)])
           | _   -> (stack', [X86Mov (s, x86eax); X86Mov(x86eax, M x); X86Add (L word_size, x86esp)])
           in res
         | S_ADD  ->
           let y::x::stack' = stack in
           let res = match x with
           | R _ -> (x::stack', [X86Add (y, x)] @ x86subStack y)
           | _   -> (x::stack', [X86Mov (x, x86eax); X86Add (y, x86eax); X86Mov (x86eax, x)] @ x86subStack y)
           in res
         | S_SUB  ->
           let y::x::stack' = stack in
           let res = match x with
           | R _ -> (x::stack', [X86Sub (y, x)] @ x86subStack y)
           | _   -> (x::stack', [X86Mov (x, x86eax); X86Sub(y, x86eax); X86Mov (x86eax, x)] @ x86subStack y)
           in res
         | S_MUL  ->
           let y::x::stack' = stack in
           let res = match x with
           | R _ -> (x::stack', [X86Mul (y, x)] @ x86subStack y)
           | _   -> (x::stack', [X86Mov (x, x86eax); X86Mul (y, x86eax); X86Mov (x86eax, x)] @ x86subStack y)
           in res
         | S_DIV  ->
           let y::x::stack' = stack in
           (x::stack', [X86Mov (x, x86eax); X86Div y; X86Mov (x86eax, x)])
         | S_MOD  ->
           let y::x::stack' = stack in
           (x::stack', [X86Mov (x, x86eax); X86Div y; X86Mov (x86edx, x)])
       in
       x86code @ x86compile' stack' code'
  in
  (*store stack base in %rbx*)
  (X86Mov (x86esp, x86ebp))::x86compile' [] code

