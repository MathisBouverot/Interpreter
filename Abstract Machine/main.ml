#load "instruction.cmo";;
#load "env.cmo";;
#load "cellstack.cmo";;
#load "evaluator.cmo";;

open Evaluator;;
open Instruction;;
let code = [Bool true; Bool true; EqBool] 
in
let rec reduce state = function
  | 0 -> state
  | n -> reduce (Evaluator.eval state) (n-1)
in 
  reduce (Cellstack.empty, Env.empty, code) 15
