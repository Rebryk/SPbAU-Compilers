open Types

let stack_run input code =
  let find_label label =
    let rec find_label' code' = 
      match code' with
      | []                        -> assert false
      | (S_LABEL label')::code''  -> if label' == label then code'' else find_label' code'' 
      | _::code''                 -> find_label' code''
    in find_label' code
  in

  let rec stack_run' (state, stack, input, output) code =
    let to_bool x   = x != 0 in
    let to_int  x   = if x then 1 else 0 in

    let run_unary_operation operation = 
      match stack with
      | []        -> assert false
      | y::stack' -> 
        let result = 
          match operation with
          | Not   -> to_int (not (to_bool y))
          | _     -> assert false
        in (state, result::stack', input, output)
    in

    let run_binary_operation operation =
      match stack with
      | [] | _::[]    -> assert false
      | y::x::stack'  ->     
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
        in (state, result::stack', input, output)
    in

    let run_comparison stack_operation =
      let result =
        match stack with
        | [] | _::[]    -> assert false
        | y::x::stack'  ->
          let result =
            match stack_operation with
            | Less      -> to_int (x < y) 
            | Leq       -> to_int (x <= y) 
            | Equal     -> to_int (x == y) 
            | Geq       -> to_int (x >= y) 
            | Greater   -> to_int (x > y) 
            | Neq       -> to_int (x != y) 
            | _         -> assert false
          in (state, result::stack', input, output)
      in result
    in

    match code with
    | []                  -> output
    | (S_JUMP l)::code'   ->
      stack_run' (state, stack, input, output) (find_label l)
    | (S_CJUMP ("Z", l))::code'  ->
        (match stack with
        | []        -> assert false
        | x::stack' -> 
          if x == 0 then 
            stack_run' (state, stack', input, output) (find_label l) 
          else 
            stack_run' (state, stack', input, output) code')
    | (S_CJUMP ("NZ", l))::code'  ->
        (match stack with
        | []        -> assert false
        | x::stack' -> 
          if x != 0 then 
            stack_run' (state, stack', input, output) (find_label l) 
          else 
            stack_run' (state, stack', input, output) code')
    | i::code' ->
       stack_run'
         (match i with
          | S_READ    ->
            let result =
              match input with
              | []        -> assert false
              | y::input' -> (state, y::stack, input', output)
            in result
          | S_WRITE   ->
            let result =
              match stack with
              | []        -> assert false
              | y::stack' -> (state, stack', input, output @ [y])
            in result
          | S_PUSH n  ->
            (state, n::stack, input, output)
          | S_LD x    ->
            (state, (List.assoc x state)::stack, input, output)
          | S_ST x    ->
            let result =
              match stack with
              | []        -> assert false
              | y::stack' -> ((x, y)::state, stack', input, output)
            in result
          | S_UNARY_OPERATION   op  -> run_unary_operation op
          | S_BINARY_OPERATION  op  -> run_binary_operation op
          | S_COMPARISON        op  -> run_comparison op
          | S_LABEL             l   -> (state, stack, input, output)
          | _                       -> assert false
         )
         code'
  in  
  stack_run' ([], [], input, []) code
