open Expr

(*
read (x);
read (y);
z := x * x;
write (z+y)
*)
let p =
  Seq (
      Seq (              
          Read "x",
          Read "y"
      ),
      Seq (
          Seq (
              Assign ("x1", Div (Var "x", Var "y")),
              Assign ("y1", Mod (Var "x", Var "y"))
          ),
          Seq(
            Write (Var "x1"),
            Write (Var "y1")
          )
      )
    )

(*
let _ =
  let [r] = run [3; 4] p in
  Printf.printf "%d\n" r
*)
(*
let ( !! )       = (!)
let ( !  ) x     = Var x
let ( $  ) n     = Const n
let ( +  ) e1 e2 = Add (e1, e2)
let ( *  ) e1 e2 = Mul (e1, e2)

let skip     = Skip
let (:=) x e = Assign (x, e)
let read x   = Read x
let write x  = Write x
let (|>) l r = Seq (l, r)
*)
(*
read (x);
read (y);
z := x * x;
write (z+y)
*)
(*
let p =
  read "x" |>
  read "y" |>
  ("z" := !"x" * !"x" * !"y" + !"x" * !"y" + !"y") |>
  write (!"z" + !"y")  
*)
(*
let _ =
  let [r] = run [3; 4] p in
  Printf.printf "%d\n" r
                
let run input p =
  srun input (compile_stmt p)

let _ =
  let [r] = run [3; 4] p in
  Printf.printf "%d\n" r
*)
let rec pr_op opnd =
	match opnd with
	| R n -> x86regs.(n)
	| S o -> Printf.sprintf "%d(%s)" ((-4) * o) (pr_op x86ebp)
	| M s -> s
	| L n -> Printf.sprintf "$%d" n

let _ =
	let vars = collect_vars p in
	let code = x86compile (compile_stmt p) in

  Printf.printf "\t.extern read\n\t.extern write\n\t.global main\n\n\t.text\n";
  Printf.printf "main:\n";
  List.iter (fun instr ->
    match instr with
    | X86Add (o1, o2) -> Printf.printf "\tADDL\t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Sub (o1, o2) -> Printf.printf "\tSUBL\t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Mul (o1, o2) -> Printf.printf "\tIMULL\t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Div o1       -> Printf.printf "\tCLTD\n\tIDIVL %s\n" (pr_op o1)
    | X86Mov (o1, o2) -> Printf.printf "\tMOVL\t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Push o1      -> Printf.printf "\tPUSHL\t%s\n" (pr_op o1)
    | X86Pop o1       -> Printf.printf "\tPOPL\t%s\n" (pr_op o1)
    | X86Call s       -> Printf.printf "\tCALL\t%s\n" s
    | X86Ret          -> Printf.printf "\tRET\n"
  ) code;
  Printf.printf "\tXORL\t%s,\t%s\nRET\n\n" (pr_op x86eax) (pr_op x86eax);
  SS.iter (fun var ->
    Printf.printf "\t.comm %s 4\n" var
  ) vars
