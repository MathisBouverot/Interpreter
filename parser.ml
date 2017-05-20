

open Ast;;

exception Parse_error of string;;

let binop_prec = Hashtbl.create 30;;
(* boolean operators *)
Hashtbl.add binop_prec "||" 10;;
Hashtbl.add binop_prec "&&" 10;;
(* int relational operators *)
Hashtbl.add binop_prec "<" 20;;
Hashtbl.add binop_prec "<=" 20;;
Hashtbl.add binop_prec ">=" 20;;
Hashtbl.add binop_prec ">" 20;;
Hashtbl.add binop_prec "==" 20;;
Hashtbl.add binop_prec "<>" 20;;
(* float relational operators *)
Hashtbl.add binop_prec "<." 20;;
Hashtbl.add binop_prec "<=." 20;;
Hashtbl.add binop_prec ">=." 20;;
Hashtbl.add binop_prec ">." 20;;
Hashtbl.add binop_prec "==." 20;;
Hashtbl.add binop_prec "<>." 20;;
(* int arithmetic operators *)
Hashtbl.add binop_prec "-" 30;;
Hashtbl.add binop_prec "+" 30;;
Hashtbl.add binop_prec "*" 40;;
Hashtbl.add binop_prec "/" 40;;
Hashtbl.add binop_prec "%" 50;;
Hashtbl.add binop_prec "**" 60;;
(* float arithmetic operators *)
Hashtbl.add binop_prec "-." 30;;
Hashtbl.add binop_prec "+." 30;;
Hashtbl.add binop_prec "*." 40;;
Hashtbl.add binop_prec "/." 40;;


let precedence op = try Hashtbl.find binop_prec op with Not_found -> -1;;


let rec parse_exp = parser
    | [< e1 = parse_primary; stream >] -> parse_arith_exp e1 stream

and parse_arith_exp lhs = parser
    | [< 'Token.BinOp op1; rhs = parse_primary; stream >] -> begin
        match Stream.peek stream with
            | Some(Token.BinOp op2) ->
                let rhs =
                    if precedence op1 < precedence op2
                    then parse_arith_exp rhs stream
                    else rhs
                in
                parse_arith_exp (BinOp (op1, lhs, rhs)) stream
            | _ -> BinOp (op1, lhs, rhs)
    end
    | [< >] -> lhs



(* primary_exp
   ::= primary_inner+ *)
and parse_primary = parser
    | [< e = parse_primary_inner; stream >] -> parse_app e stream

and parse_app callee = parser
    | [< arg = parse_primary_inner; stream >] -> parse_app (App (callee, arg)) stream
    | [< >] -> callee

and parse_tuple acc = parser
    | [< 'Token.Punct ','; e = parse_exp; stream >] ->
        parse_tuple (e :: acc) stream
    | [< >] -> Tuple (List.rev acc)

and parse_seq acc = parser
    | [< 'Token.Punct ';'; e = parse_exp; stream >] ->
        parse_seq (e :: acc) stream
    | [< >] -> Seq (List.rev acc)

(* primary_inner
   ::= boolexp
   ::= intexp
   ::= floatexp
   ::= stringexp
   ::= '(' tupexp ')' | '(' seqexp ')'
   ::= varexp
   ::= typeinexp
   ::= 'if' exp 'then' exp 'else' exp
   ::= unary_op primary_inner *)
and parse_primary_inner = parser
    | [< 'Token.Bool b >] -> Bool b
    | [< 'Token.Int n >] -> Int n
    | [< 'Token.Float f >] -> Float f
    | [< 'Token.String s >] -> String s
    | [< 'Token.Ident id >] -> Var id
    | [< 'Token.UnOp op >] -> Var op
    (* tuples and sequence expressions *)
    | [< 'Token.Punct '('; e = parse_exp; stream >] -> begin
        match Stream.peek stream with
            | Some (Token.Punct ',') ->
                let tup = parse_tuple [e] stream in
                    begin parser
                        | [< 'Token.Punct ')' >] -> tup
                        | [< >] -> raise @@ Failure "unatched '('"
                    end stream
            | Some (Token.Punct ';') ->
                let seq = parse_seq [e] stream in
                    begin parser
                        | [< 'Token.Punct ')' >] -> seq
                        | [< >] -> raise @@ Failure "unmatched '('"
                    end stream
            | _ ->
                begin parser
                    | [< 'Token.Punct ')' >] -> e
                    | [< >] -> raise @@ Failure "unmatched '('"
                end stream
    end
    (* conditional expression *)
    | [< 'Token.If; pred = parse_exp;
        'Token.Then ?? "expected 'then' after 'if'"; e1 = parse_exp;
        'Token.Else ?? "expected 'else' after 'then'"; e2 = parse_exp >] ->
        Cond (pred, e1, e2)
    (* funexp
        ::= Lambda '(' id ':' tid ')' ':' tid '->' expr args* *)
    | [< 'Token.Lambda ; 'Token.Punct '(' ?? "expected '(' after '\'";
        args = parse_args [];
        'Token.Punct ')' ?? "expected ')' after type identifier in lambda expression";
        'Token.Punct ':' ?? "expected ':' after ')' in lambda expression";
        te = parse_type_exp ?? "expected a type expression after ':' in lambda expression";
        'Token.BinOp "=>" ?? "2expected '=>' after return type expression in lambda expression";
        e = parse_exp >] ->
        let ids, types = List.split args in
            MultiClosure(ids, types, e, te)
    (* varexp
        ::= ('var' bindexp 'and')* 'var' bindexp 'in' exp *)
    | [< 'Token.Var; flags = parse_flags []; e = parse_bindexp; stream >] ->
        begin parser
            | [< 'Token.In; e2 = parse_exp >] -> BindIn (flags, e, e2)
            | [< >] -> raise @@ Failure "expected 'in' after expression in var binding"
        end stream
    | [< 'Token.Type; e1 = parse_type_bindexp; stream >] ->
        begin parser
            | [< 'Token.In; e2 = parse_exp >] -> TypeIn (e1, e2)
            | [< >] -> raise @@ Failure "expected 'in' after type expression in type definition"
        end stream


and parse_args acc = parser
    | [< 'Token.Ident id;
        'Token.Punct ':' ?? "expected ':' after identifier in lambda expression";
        te = parse_type_exp ?? "expected a type expression after ':' in lambda expression";
        stream >] ->
        parse_args ((id, te) :: acc) stream
    | [< 'Token.Punct ',';
        stream >] ->
        parse_args acc stream
    | [< >] -> List.rev acc

(* flags
    ::= ('rec'|'lazy')* *)
and parse_flags acc = parser
    | [< 'Token.Rec; stream >] ->
        if List.mem Rec acc
        then raise @@ Failure "duplicate flag 'rec'"
        else parse_flags (Rec :: acc) stream
    | [< >] -> acc

(* bindexp
   ::= id '=' exp *)
and parse_bindexp = parser
    | [< 'Token.Ident id;
        'Token.Punct '=' ?? "expected '=' after '" ^ id ^ "' in var expression";
        e = parse_exp >] ->
        Bind (id, e)
    | [< >] -> raise @@ Failure "expected identifier before '=' in var expression"

(* typebindexp
   ::= tid '=' typeexp *)
and parse_type_bindexp = parser
    | [< 'Token.TIdent tid;
        'Token.Punct '=' ?? "expected '=' after '" ^ tid ^ "' in type definition expression";
        te = parse_type_exp >] ->
        TypeBind (tid, te)
    | [< >] ->
        raise @@ Failure "expected type identifier before '=' in type definition expression"

and parse_type_exp = parser
    | [< te1 = parse_tuple_type []; stream >] ->
        (* get rid of single element tuples *)
        let te1 =
            match te1 with
                | Types.Tuple (t :: []) -> t
                | _ -> te1
        in
        begin parser
            | [< 'Token.BinOp "->" >] ->
                let te2 = parse_type_exp stream in
                    Types.Function (te1, te2)
            | [< >] -> te1
        end stream
    | [< >] -> raise @@ Failure "invalid type expression"

and parse_tuple_type acc = parser
    | [< e = parse_primary_type; stream >] ->
        let acc' = e :: acc in
            parse_tuple_type acc' stream
    | [< 'Token.BinOp "*"; stream >] ->
        parse_tuple_type acc stream
    | [< >] -> Types.Tuple (List.rev acc)

and parse_primary_type = parser
    | [< 'Token.Punct '('; e = parse_type_exp; 'Token.Punct ')' ?? "unmatched '('" >] -> e
    | [< 'Token.TIdent tid >] -> type_of_tid tid

and type_of_tid = function
    | "'bool" -> Types.Bool
    | "'int" -> Types.Int
    | "'float" -> Types.Float
    | "'string" -> Types.String
    | t -> raise @@ Failure ("invalid type " ^ t)
