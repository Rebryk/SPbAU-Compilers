module SS = Set.Make(String)

open Types
open StackCompiler

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
  | _               -> assert false

let x86regs = [|"%esp"; "%ebp"; "%eax"; "%edx"; "%ecx"; "%ebx"; "%esi"; "%edi"|]
let x86small_regs = [|"%al"; "%dl"; "%bl"; "%cl"|]

let num_of_regs = Array.length x86regs
let num_of_small_regs = Array.length x86small_regs

let word_size = 4

let x86esp = R 0
let x86ebp = R 1
let x86eax = R 2
let x86edx = R 3

let x86al = S_R 0
let x86dl = S_R 1
let x86bl = S_R 2 
let x86cl = S_R 3

let allocate stack =
  match stack with
  | []                              -> R 4 (* preserve esp, ebp, eax and edx for stuff :) *)
  | (S n)::_                        -> S (n+1)
  | (R n)::_ when n < num_of_regs-1 -> R (n+1)
  | _                               -> S 0

let x86compile : stack_instruction list -> x86instruction list = fun code ->
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

  let rec x86compile' stack code label =
    let compile_comparison operation stack code label =
      match stack with
      | [] | _::[]    -> assert false
      | y::x::stack'  ->
        let result =
          match x with
          | R _ -> (x::stack', [X86Cmp (y, x); X86Mov (L 1, x); X86Jump (operation, label); X86Mov (L 0, x); X86Label label] @ x86subStack y, label)
          | _   -> (x::stack', [X86Mov (x, x86eax); X86Cmp (y, x86eax); X86Mov (L 1, x86eax); X86Jump (operation, label); X86Mov (L 0, x86eax); X86Label label] @ x86subStack y, label)
        in result
    in

    let compile_binary_operation operation stack code label = 
      match stack with
      | [] | _::[]    -> assert false
      | y::x::stack'  ->  
        let result = 
          match x with
          | R _ -> (x::stack', [X86BinaryOperation (operation, y, x)] @ x86subStack y, label)
          | _   -> (x::stack', [X86Mov (x, x86eax); X86BinaryOperation (operation, y, x86eax); X86Mov (x86eax, x)] @ x86subStack y, label)
        in result
    in
    
    let compile_logical operation stack code label =
      match stack with
      | [] | _::[]    -> assert false
      | y::x::stack'  ->
        let to_bool x = [X86Mov (x, x86eax); X86Cmp (L 0, x86eax); X86Mov (L 0, x86eax); X86Setne x86al; X86Mov (x86eax, x)] in
        let (stack', code', label') = compile_binary_operation operation stack code label in
        (stack', to_bool x @ to_bool y @ code', label')
    in

    let compile_unary_operation operation stack code label =
      match stack with
      | []        -> assert false
      | x::stack' -> (stack, [X86UnaryOperation (operation, x)], label)
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
          let result = 
            match stack with
            | []        -> assert false
            | s::stack' -> (stack', [X86Push s; X86Call "write"] @ x86subStack (S 0) @ x86subStack s, label)
          in result
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
          let result = 
            match stack with
            | []        -> assert false
            | s::stack' ->
              let result = 
                match s with
                | R _ -> (stack', [X86Mov (s, M x)], label)
                | _   -> (stack', [X86Mov (s, x86eax); X86Mov(x86eax, M x); X86BinaryOperation (Add, L word_size, x86esp)], label)
              in result
          in result
        | S_UNARY_OPERATION op    -> compile_unary_operation op stack code label
        | S_BINARY_OPERATION Div  ->
          let result =
            match stack with
            | [] | _::[]    -> assert false
            | y::x::stack'  -> (x::stack', [X86Mov (x, x86eax); X86Div y; X86Mov (x86eax, x)], label)
          in result
        | S_BINARY_OPERATION Mod  ->
          let result =
            match stack with
            | [] | _::[]    -> assert false
            | y::x::stack'  -> (x::stack', [X86Mov (x, x86eax); X86Div y; X86Mov (x86edx, x)], label)
          in result
        | S_BINARY_OPERATION  Or  -> compile_logical Or stack code label
        | S_BINARY_OPERATION  And -> compile_logical And stack code label
        | S_BINARY_OPERATION  op  -> compile_binary_operation op stack code label
        | S_COMPARISON        op  -> compile_comparison op stack code (label + 1)
       in
       x86code @ x86compile' stack' code' label'
  in
  (X86Mov (x86esp, x86ebp))::x86compile' [] code 0

let rec pr_op opnd =
  match opnd with
  | S_R n -> x86small_regs.(n)
  | R n   -> x86regs.(n)
  | S o   -> Printf.sprintf "%d(%s)" ((-4) * o) (pr_op x86ebp)
  | M s   -> s
  | L n   -> Printf.sprintf "$%d" n

let gen_asm p name =
  let vars = collect_vars p in
  let code = x86compile (compile_statement p) in
  let outf = open_out (Printf.sprintf "%s" name) in
  
  Printf.fprintf outf "\t.extern read\n\t.extern write\n\t.global main\n\n\t.text\n";
  Printf.fprintf outf "main:\n";
  List.iter (fun instr ->
    match instr with
    | X86Div    o1                      -> Printf.fprintf outf "\tCLTD\n\tIDIVL\t%s\n" (pr_op o1)
    | X86Mov    (o1, o2)                -> Printf.fprintf outf "\tMOVL\t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Cmp    (o1, o2)                -> Printf.fprintf outf "\tCMPL\t%s,\t%s\n" (pr_op o1) (pr_op o2) 
    | X86Push   o1                      -> Printf.fprintf outf "\tPUSHL\t%s\n" (pr_op o1)
    | X86Pop    o1                      -> Printf.fprintf outf "\tPOPL\t%s\n" (pr_op o1)
    | X86Call   s                       -> Printf.fprintf outf "\tCALL\t%s\n" s
    | X86Ret                            -> Printf.fprintf outf "\tRET\n"
    | X86Label  n                       -> Printf.fprintf outf "label%d:\n" n
    | X86UnaryOperation (op, o)         -> Printf.fprintf outf "\tNOTL\t%s\n" (pr_op o)
    | X86BinaryOperation (Add, o1, o2)  -> Printf.fprintf outf "\tADDL\t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86BinaryOperation (Sub, o1, o2)  -> Printf.fprintf outf "\tSUBL\t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86BinaryOperation (Mul, o1, o2)  -> Printf.fprintf outf "\tIMULL\t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86BinaryOperation (And, o1, o2)  -> Printf.fprintf outf "\tANDL\t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86BinaryOperation (Or, o1, o2)   -> Printf.fprintf outf "\tORL\t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Jump   (Less, n)               -> Printf.fprintf outf "\tJL\tlabel%d\n" n
    | X86Jump   (Leq, n)                -> Printf.fprintf outf "\tJLE\tlabel%d\n" n
    | X86Jump   (Equal, n)              -> Printf.fprintf outf "\tJE\tlabel%d\n" n
    | X86Jump   (Geq, n)                -> Printf.fprintf outf "\tJGE\tlabel%d\n" n
    | X86Jump   (Greater, n)            -> Printf.fprintf outf "\tJG\tlabel%d\n" n
    | X86Jump   (Neq, n)                -> Printf.fprintf outf "\tJNE\tlabel%d\n" n
    | X86Setne  o1                      -> Printf.fprintf outf "\tSETNE\t%s\n" (pr_op o1)
    | _                                 -> assert false
  ) code;
  Printf.fprintf outf "\tXORL\t%s,\t%s\nRET\n\n" (pr_op x86eax) (pr_op x86eax);
  SS.iter (fun var -> Printf.fprintf outf "\t.comm %s 4\n" var) vars;
  close_out outf
