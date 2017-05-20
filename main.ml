#load "lexer.cmo";;
#load "parser.cmo";;
#load "env.cmo";;
#load "passes.cmo";;
#load "codegen.cmo";;
#load "cell.cmo";;
#load "memory.cmo";;
#load "evaluator.cmo";;


open Instruction;;

let rec stack_to_list stack acc = 
  if Stack.is_empty stack then acc 
  else
    let top = Stack.pop stack in
      stack_to_list stack (top :: acc);;


let file = "prog.txt" in
let rec read_lines ?(str = "") in_chan =
  try
    let str = str ^ (input_line in_chan) ^ "\n" in
      read_lines in_chan ~str:str
  with
    | End_of_file -> str
    | e -> close_in_noerr in_chan; raise e
in
let in_chan = open_in file in
let str = read_lines in_chan in
  close_in in_chan;
  print_string str;
  flush stdout;
  let tokens = Lexer.lex (Stream.of_string str) in
  let e = Parser.parse_exp tokens in
  let e = Passes.all_passes e in
  let code = Codegen.translate_prog e in
  let s, m, c = Evaluator.reduce (Stack.create (), Memory.create 10 Cell.Triv, code) 30 in
    stack_to_list s [], m, c
