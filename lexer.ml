
(* global variable for line number,
   only changed using inc_line () function*)
let line = ref 0
let inc_line () = line := !line + 1

exception Parse_error of string

let rec lex = parser
    (* skip white space *)
    | [< ''\n'; stream >] -> (inc_line (); lex stream)
    | [< ' (' ' | '\t' | '\r' ); stream = lex >] -> stream
    (* ident ::= [a-z][a-zA-Z0-9_]* *)
    | [< ' ('a' .. 'z'  as head); stream >] -> begin
        let buff = Buffer.create 10 in
            Buffer.add_char buff head;
            lex_ident buff stream
    end
    (* module ident ::= [A-Z][a-zA-Z0-9_]* *)
    | [< ' ('A' .. 'Z' as head); stream >] -> begin
        let buff = Buffer.create 10 in
            Buffer.add_char buff head;
            lex_mident buff stream
    end
    (* type ident ::= [''][a-zA-Z0-9_]* *)
    | [< ''\''; stream >] -> begin
        let buff = Buffer.create 10 in
            Buffer.add_char buff '\'';
            lex_tident buff stream
    end
    (* string *)
    | [< ' ('"'); stream >] -> lex_string (Buffer.create 10) stream
    (* float
        ::= [0-9]+ '.' [0-9]*
        ::= [0-9]* '.' [0-9]+
   int
        ::= [0-9]+ *)
    | [< ' ('0' .. '9' | '.' as head); stream >] -> begin
        let buff = Buffer.create 10 in Buffer.add_char buff head;
        let read_dot, read_digit = (if head = '.' then true, false else false, true) in
            lex_number buff read_dot read_digit stream
    end
    (* lambda
        ::= '\' *)
    | [< ''\\'; stream = lex >] -> [< 'Token.Lambda; stream >]
    (* bin_op
        ::= '/' | '/.'
        ::= '*' | '*.'
        ::= '+' | '+.'
        ::= '-' | '-.'
        ::= '%'
        ::= '<-'
        ::= '->'*)
    | [< ''%'; stream >] -> [< 'Token.BinOp "%"; lex stream >]
    | [< ' ('/' | '*' | '+' as head); stream >] ->
        begin parser
            | [< ''.'; stream = lex >] -> [< 'Token.BinOp ((String.make 1 head) ^ "."); stream >]
            | [< stream = lex >] -> [< 'Token.BinOp (String.make 1 head); stream >]
        end stream
    | [< ''-'; stream >] ->
        begin parser
            | [< ''>'; stream = lex>] -> [< 'Token.BinOp "->"; stream >]
            | [< ''<'; stream = lex_multiple_comment 1 >] -> [< stream >]
            | [< ''-'; stream = lex_single_comment >] -> [< stream >]
            | [< ''.'; stream = lex >] -> [< 'Token.BinOp "-."; stream >]
            | [< stream = lex >] -> [< 'Token.BinOp "-"; stream >]
        end stream
    (* rel_op
        ::= '<'  | '<.'
        ::= '<=' | '<=.'
        ::= '==' | '==.'
        ::= '!=' | '<>.'
        ::= '>=' | '>=.'
        ::= '>'  | '>.' *)
    | [< ''<'; stream >] ->
        begin parser
            | [< ''='; stream >] ->
                begin parser
                    | [< ''.'; stream = lex >] -> [< 'Token.BinOp "<=."; stream >]
                    | [< stream = lex >] -> [< 'Token.BinOp "<="; stream >]
                end stream
            | [< ''-'; stream = lex >] -> [< 'Token.BinOp "<-"; stream >]
            | [< ''>'; stream >] ->
                begin parser
                    | [< ''.'; stream = lex >] -> [< 'Token.BinOp "<>."; stream >]
                    | [< stream = lex >] -> [< 'Token.BinOp "<>"; stream >]
                end stream
            | [< ''.'; stream = lex >] -> [< 'Token.BinOp "<."; stream >]
            | [< >] -> [< 'Token.BinOp "<"; lex stream >]
        end stream
    | [< ''>'; stream >] ->
        begin parser
            | [< ''='; stream >] ->
                begin parser
                    | [< ''.'; stream = lex >] -> [< 'Token.BinOp ">=."; stream >]
                    | [< stream = lex >] -> [< 'Token.BinOp ">="; stream >]
                end stream
            | [< ''.'; stream = lex >] -> [< 'Token.BinOp ">."; stream >]
            | [< >] -> [< 'Token.BinOp ">"; lex stream >]
        end stream
    | [< ''='; stream >] ->
        begin parser
            | [< ''='; stream >] ->
                begin parser
                    | [< ''.'; stream = lex >] -> [< 'Token.BinOp "==."; stream >]
                    | [< stream = lex >] -> [< 'Token.BinOp "=="; stream >]
                end stream
            | [< >] -> [< 'Token.Punct '='; lex stream >]
        end stream
    (* bool_op *)
    | [< ''!'; stream = lex >] -> [< 'Token.UnOp "!"; stream >]
    | [< ''&'; ''&'; stream = lex >] -> [< 'Token.BinOp "&&"; stream >]
    | [< ''|'; stream >] ->
        begin parser
            | [< ''|'; stream = lex >] -> [< 'Token.BinOp "||"; stream >]
            | [< stream = lex >] -> [< 'Token.Punct '|'; stream >]
        end stream
    (* punctuation *)
    | [< ' ('(' | ')' | ',' | ';' as head); stream = lex >] -> [< 'Token.Punct head; stream >]
    | [< '':'; stream >] -> begin
        match Stream.peek stream with
            | Some ':' -> let _ = Stream.junk stream in [< 'Token.BinOp "::"; lex stream >]
            | _ -> [< 'Token.Punct ':'; lex stream >]
    end
    (* unknown token *)
    | [< 'head; stream = lex>] -> [< 'Token.Kwd head; stream >]
    (* end of file *)
    | [< >] -> [< >]


and lex_ident buff = parser
    | [< ' ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' as head); stream >] ->
        Buffer.add_char buff head;
        lex_ident buff stream
    | [< stream >] -> match Buffer.contents buff with
        | "if" -> [< 'Token.If; lex stream >]
        | "then" -> [< 'Token.Then; lex stream >]
        | "else" -> [< 'Token.Else; lex stream >]
        | "var" -> [< 'Token.Var; lex stream >]
        | "in" -> [< 'Token.In; lex stream >]
        | "and" -> [< 'Token.And; lex stream >]
        | "rec" -> [< 'Token.Rec; lex stream >]
        | "lazy" -> [< 'Token.Lazy; lex stream >]
        | "receive" -> [< 'Token.Receive; lex stream >]
        | "after" -> [< 'Token.After; lex stream >]
        | "module" -> [< 'Token.Module; lex stream >]
        | "end" -> [< 'Token.End; lex stream >]
        | "true" -> [< 'Token.Bool true; lex stream >]
        | "false" -> [< 'Token.Bool false; lex stream >]
        | id -> [< 'Token.Ident id; lex stream >]


and lex_tident buff = parser
    | [< ' ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' as head); stream >] ->
        Buffer.add_char buff head;
        lex_tident buff stream
    | [< stream >] -> [< 'Token.TIdent (Buffer.contents buff); lex stream >]


and lex_mident buff =  parser
    | [< ' ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' as head); stream >] ->
        Buffer.add_char buff head;
        lex_mident buff stream
    | [< stream >] -> [< 'Token.MIdent (Buffer.contents buff); lex stream >]


and lex_number buff read_dot read_digit = parser
    | [< ' ('0' .. '9' as head); stream >] ->
        Buffer.add_char buff head;
        lex_number buff read_dot true stream
    | [< ''.'; stream >] ->
        Buffer.add_char buff '.';
        if read_dot = true then raise @@ Parse_error "a number can't contain more than a single '.'"
        else lex_number buff true read_digit stream
    | [< stream = lex >] ->
        if read_dot then [< 'Token.Float (float_of_string (Buffer.contents buff)); stream >]
        else [< 'Token.Int (int_of_string (Buffer.contents buff)); stream >]


and lex_string buff = parser
    | [< ' ('"'); stream = lex >] -> [< 'Token.String (Buffer.contents buff); stream >]
    | [< 'chr; stream >] -> begin
        Buffer.add_char buff chr;
        lex_string buff stream
    end


and lex_single_comment = parser
    | [< ''\n'; stream = lex >] -> (inc_line (); stream)
    | [< 'chr; stream = lex_single_comment >] -> stream


and lex_multiple_comment n = parser
    (* close comment *)
    | [< ''>'; stream >] -> begin
        match Stream.peek stream with
            | Some('-') -> begin
                Stream.junk stream;
                if n > 1 then lex_multiple_comment (n-1) stream
                else lex stream
            end
            | _ -> lex_multiple_comment n stream
    end
    (* open comment *)
    | [< ''-'; stream >] -> begin
        match Stream.peek stream with
            | Some('<') -> (Stream.junk stream; lex_multiple_comment (n+1) stream)
            | _ -> lex_multiple_comment n stream
    end
    (* new lines *)
    | [< ''\n'; stream >] -> (inc_line (); lex_multiple_comment n stream)
    | [< 'chr; stream = lex_multiple_comment n >] -> stream
