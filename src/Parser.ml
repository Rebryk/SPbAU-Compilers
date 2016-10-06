open Types
open Ostap
open Matcher

ostap (
  expression: op5;
  
  op5:
    l:op4 tail:("!!" r:op4)* {List.fold_left (fun l (op, r) -> BinaryOperation (Or, l, r)) l tail};

  op4:
    l:op3 tail:("&&" r:op3)* {List.fold_left (fun l (op, r) -> BinaryOperation (And, l, r)) l tail};

  op3:
    l:op2 tail:(("<=" | "<" | "==" | "!=" | ">=" | ">") r:op2)* {
      List.fold_left
      (fun l (operation, r) ->
        match operation with
        | ("<", _)  -> Comparison (Less,      l, r)
        | ("<=", _) -> Comparison (Leq,       l, r)
        | ("==", _) -> Comparison (Equal,     l, r)
        | ("!=", _) -> Comparison (Neq,       l, r)
        | (">=", _) -> Comparison (Geq,       l, r)
        | (">", _)  -> Comparison (Greater,   l, r)
        | _         -> assert false)
      l tail}; 

  op2:
    l:op1 tail:(("+" | "-") r:op1)* {
      List.fold_left
      (fun l (operation, r) ->
        match operation with
        | ("+", _)  -> BinaryOperation (Add, l, r)
        | ("-", _)  -> BinaryOperation (Sub, l, r)
        | _         -> assert false)
      l tail};

  op1:
    l:primary tail:(("*" | "/" | "%") r:primary)* {
      List.fold_left 
      (fun l (operation, r) ->
        match operation with
        | ("*", _)  -> BinaryOperation (Mul, l, r)
        | ("/", _)  -> BinaryOperation (Div, l, r)
        | ("%", _)  -> BinaryOperation (Mod, l, r)
        | _         -> assert false)
      l tail}
    | primary; 

  primary:
    c:DECIMAL   { Const  c }
    | x:IDENT   { Var    x }
    | -"(" expression -")";

  statement:
    s1:simple ";" s2:statement { Seq (s1, s2) }
    | simple;

  simple:
    %"read" "(" name:IDENT ")"        { Read   name   }
    | %"write" "(" e:expression ")"   { Write  e      }
    | %"skip"                         { Skip          }
    | x:IDENT ":=" e:expression       { Assign (x, e) }
)

let parse input_file = 
  let code = Util.read input_file in
  Util.parse 
    (object
      inherit Matcher.t code
      inherit Util.Lexers.ident ["read"; "write"; "skip"] code
      inherit Util.Lexers.decimal code
      inherit Util.Lexers.skip [
        Matcher.Skip.whitespaces " \t\n";
        Matcher.Skip.lineComment "--";
        Matcher.Skip.nestedComment "(*" "*)"
      ] code
    end)
    (ostap (statement -EOF))

