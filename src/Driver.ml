open Types
open X86Compiler

let build code name = 
  gen_asm code (Printf.sprintf "%s.S" name);
  let runtime_dir = try 
    Sys.getenv "RC_RUNTIME"
    with Not_found -> "../runtime"
  in
  Sys.command (Printf.sprintf "gcc -m32 -o %s %s/runtime.o %s.S" name runtime_dir name)

let _ = 
  try
    let mode, filename = 
      match Sys.argv.(1) with
      | "-s"  -> `SM,  Sys.argv.(2)
      | "-o"  -> `X86, Sys.argv.(2)
      | "-i"  -> `Int, Sys.argv.(2)
      | _     -> assert false
    in

    match Parser.parse filename with
    | `Ok code ->
       (
         match mode with
         | `X86 -> ignore @@ build code (Filename.chop_suffix filename ".expr")
         | _    ->
            let rec read acc =
              try
                let r = read_int() in
                Printf.printf "> ";
                read (acc @ [r])
              with End_of_file -> acc
            in

            let input = read [] in
            let output = 
              let code' = StackCompiler.compile_statement code in
              (* let code' = [S_READ; S_ST "x"; S_PUSH 0; S_LABEL 2; S_WRITE; S_JUMP 1; S_LD "x"; S_WRITE] in*)

              match mode with
              | `SM -> StackInterpreter.stack_run input code'
              | _   -> Interpreter.run input code
            in
            List.iter (fun i -> Printf.printf "%d\n" i) output
       )
    | `Fail er -> Printf.eprintf "%s" er
  with Invalid_argument _ ->
    Printf.printf "Usage: rc.byte <name.exp>\n"
