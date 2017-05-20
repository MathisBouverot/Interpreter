
#load "memory.cmo";;
#load "evaluator.cmo";;
#load "cell.cmo";;
#load "instruction.cmo";;

open Instruction;;
open Evaluator;;


let rec stack_to_list stack acc = 
  if Stack.is_empty stack then acc 
  else
    let top = Stack.pop stack in
      stack_to_list stack (top :: acc);;
     
let code = [Int 1; Bind 0; Closure ([Int 7; Access 0; AddInt; Return], 0); Bind 1; Access 1; Access 0; Apply] 
in
let rec reduce (s, m, c) = function
  | 0 -> s, m, c
  | n -> reduce (Evaluator.eval (s, m, c)) (n-1)
in
let s, m, c = reduce (Stack.create (), Memory.create 10 Cell.Triv, code) 15 in
  stack_to_list s [], m, c;;
