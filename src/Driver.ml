open Expr

let p =
  Seq (
      Seq (              
          Read "x",
          Read "y"
      ),
      Write (Mul (Var "x", Var "y"))
    )

let rec pr_op opnd =
	match opnd with
	| R n -> x86regs.(n)
	| S o -> Printf.sprintf "%d(%s)" ((-4) * o) (pr_op x86ebp)
	| M s -> s
	| L n -> Printf.sprintf "$%d" n

let gen_asm p name =
	let vars = collect_vars p in
	let code = x86compile (compile_stmt p) in
  let outf = open_out (Printf.sprintf "%s" name) in

  let get_asm_unary_operation operation operand outf =
    let pattern = 
      match operation with
      | Not -> "\tNOTL\t%s\n"
      | _   -> assert false
    in
    Printf.fprintf outf pattern (pr_op operand)
  in

  let get_asm_binary_operation operation operand1 operand2 outf =
    let pattern = 
      match operation with
      | Add -> "\tADDL\t%s,\t%s\n"
      | Sub -> "\tSUBL\t%s,\t%s\n"
      | Mul -> "\tIMULL\t%s,\t%s\n"
      | And -> "\tANDL\t%s,\t%s\n" 
      | Or  -> "\tORL\t%s,\t%s\n"
      | _   -> assert false
    in
    Printf.fprintf outf pattern (pr_op operand1) (pr_op operand2)
  in

  let get_asm_comparison operation n outf =
    let pattern = 
      match operation with
      | Less    -> "\tJL\tlabel%d\n"
      | Leq     -> "\tJLE\tlabel%d\n"
      | Equal   -> "\tJE\tlabel%d\n"
      | Geq     -> "\tJGE\tlabel%d\n"
      | Greater -> "\tJG\tlabel%d\n"
      | Neq     -> "\tJNE\tlabel%d\n"
      | _       -> assert false
    in
    Printf.fprintf outf pattern n
  in
  
  Printf.fprintf outf "\t.extern read\n\t.extern write\n\t.global main\n\n\t.text\n";
  Printf.fprintf outf "main:\n";
  List.iter (fun instr ->
    match instr with
    | X86Div    o1                    -> Printf.fprintf outf "\tCLTD\n\tIDIVL\t%s\n" (pr_op o1)
    | X86Mov    (o1, o2)              -> Printf.fprintf outf "\tMOVL\t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Cmp    (o1, o2)              -> Printf.fprintf outf "\tCMPL\t%s,\t%s\n" (pr_op o1) (pr_op o2) 
    | X86Push   o1                    -> Printf.fprintf outf "\tPUSHL\t%s\n" (pr_op o1)
    | X86Pop    o1                    -> Printf.fprintf outf "\tPOPL\t%s\n" (pr_op o1)
    | X86Call   s                     -> Printf.fprintf outf "\tCALL\t%s\n" s
    | X86Ret                          -> Printf.fprintf outf "\tRET\n"
    | X86Label  n                     -> Printf.fprintf outf "label%d:\n" n
    | X86UnaryOperation (op, o)       -> get_asm_unary_operation op o outf
    | X86BinaryOperation (op, o1, o2) -> get_asm_binary_operation op o1 o2 outf
    | X86Jump   (op, n)               -> get_asm_comparison op n outf
  ) code;
  Printf.fprintf outf "\tXORL\t%s,\t%s\nRET\n\n" (pr_op x86eax) (pr_op x86eax);
  SS.iter (fun var -> Printf.fprintf outf "\t.comm %s 4\n" var) vars;
  close_out outf

let _ = 
  gen_asm p "main.S";
  Sys.command (Printf.sprintf "gcc -m32 -o main ../runtime/runtime.o main.S")
