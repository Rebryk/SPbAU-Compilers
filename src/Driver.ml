open Expr

let p =
  Seq (
      Seq (              
          Read "x",
          Read "y"
      ),
      Write (BinaryOperation (Mul, Var "x", Var "y"))
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
  ) code;
  Printf.fprintf outf "\tXORL\t%s,\t%s\nRET\n\n" (pr_op x86eax) (pr_op x86eax);
  SS.iter (fun var -> Printf.fprintf outf "\t.comm %s 4\n" var) vars;
  close_out outf

let _ = 
  gen_asm p "main.S";
  Sys.command (Printf.sprintf "gcc -m32 -o main ../runtime/runtime.o main.S")
