#load "lexer.cmo";;
#load "parser.cmo";;



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
  let prog = Parser.parse_defs tokens in
    prog
