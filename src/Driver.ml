open Types
open X86Compiler

let build code name = 
  gen_asm code (Printf.sprintf "%s.S" name);
  Sys.command (Printf.sprintf "gcc -m32 -o %s ../runtime/runtime.o %s.S" name name)

let _ = 
  try
    let filename = Sys.argv.(1) in
    match Parser.parse filename with
    | `Ok code -> ignore @@ build code (Filename.chop_suffix filename ".expr")
    | `Fail er -> Printf.eprintf "%s" er
  with Invalid_argument _ ->
    Printf.printf "Usage: rc.byte <name.exp>\n"
