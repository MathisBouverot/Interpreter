
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


(* defs
   ::= bindexp ';' defs
   ::= epsilon *)
let rec parse_defs ?(acc = []) = parser
    | [< 'Token.Var;
        flags = parse_flags [];
        e = parse_bindexp;
        'Token.Punct ';' ?? "expected ';' after var expression"; stream >] ->
        parse_defs ~acc:((flags, e) :: acc) stream
    | [< >] -> acc


(* flags
   ::= ('rec'|'lazy')* *)
and parse_flags acc = parser
    | [< 'Token.Rec; stream >] ->
        if List.mem Ast.Rec acc
        then raise @@ Failure "duplicate flag 'rec'"
        else parse_flags (Ast.Rec :: acc) stream
    | [< 'Token.Lazy; stream >] ->
        if List.mem Ast.Lazy acc
        then raise @@ Failure "duplicate flag 'lazy'"
        else parse_flags (Ast.Lazy :: acc) stream
    | [< >] -> acc


(* bindexp
   ::= id '=' exp *)
and parse_bindexp = parser
    | [< 'Token.Ident id;
        'Token.Punct '=' ?? "expected '=' after '" ^ id ^ "' in var expression";
        e = parse_exp >] ->
        Ast.BindExp (id, e)
    | [< >] -> raise @@ Failure "expected identifier before '=' in var expression"



and parse_exp = parser
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
                parse_arith_exp (Ast.AppExp(Ast.AppExp((Ast.VarExp op1), lhs), rhs)) stream
            | _ -> Ast.AppExp(Ast.AppExp((Ast.VarExp op1), lhs), rhs)
    end
    | [< >] -> lhs




(* primary_exp
   ::= primary_inner+ *)
and parse_primary = parser
    | [< e = parse_primary_inner; stream >] -> parse_app e stream

and parse_app callee = parser
    | [< arg = parse_primary_inner; stream >] -> parse_app (Ast.AppExp (callee, arg)) stream
    | [< >] -> callee

(* primary_inner
   ::= boolexp
   ::= intexp
   ::= floatexp
   ::= stringexp
   ::= '(' exp ')'
   ::= varexp
   ::= 'if' exp 'then' exp 'else' exp
   ::= mid '::' id
   ::= unary_op primary_inner *)
and parse_primary_inner = parser
    | [< 'Token.Bool b >] -> Ast.BoolExp b
    | [< 'Token.Int n >] -> Ast.IntExp n
    | [< 'Token.Float f >] -> Ast.FloatExp f
    | [< 'Token.String s >] -> Ast.StrExp s
    | [< 'Token.Ident id >] -> Ast.VarExp id
    | [< 'Token.UnOp op >] -> Ast.VarExp op
    | [< 'Token.Punct '('; e = parse_exp; 'Token.Punct ')' ?? "unmatched '('"; stream >] -> begin
        match Stream.peek stream with
            | Some(Token.Punct '(') | Some(Token.Ident _) | Some(Token.Bool _)
            | Some(Token.Int _) | Some(Token.Float _) | Some(Token.String _) -> Ast.AppExp(e, parse_exp stream)
            | _ -> e
    end
    | [< 'Token.If; pred = parse_exp;
        'Token.Then ?? "expected 'then' after 'if'"; e1 = parse_exp;
        'Token.Else ?? "expected 'else' after 'then'"; e2 = parse_exp >] ->
        Ast.CondExp (pred, e1, e2)
    (* funexp
        ::= Lambda '(' id ':' tid ')' ':' tid '->' expr args* *)
    | [< 'Token.Lambda; 'Token.Punct '(' ?? "expected '(' after '\'";
        'Token.Ident id ?? "expected an identifier after '(' in lambda expression";
        'Token.Punct ':' ?? "expected ':' after the identifier in lambda expression";
        'Token.TIdent tid1 ?? "expected a type identifier after ':' in lambda expression";
        'Token.Punct ')' ?? "expected ')' after the first type identifier in lambda expression";
        'Token.Punct ':' ?? "expected ':' after ')' in lambda expression";
        'Token.TIdent tid2 ?? "expected a type identifier after ':' in lambda expression";
        'Token.BinOp "->" ?? "expected '->' after second type identifier in lambda expression";
        e = parse_exp >] ->
        Ast.FunExp(id, type_of_tid tid1, e, type_of_tid tid2, [])

    (* varexp
        ::= ('var' bindexp 'and')* 'var' bindexp 'in' exp *)
    | [< 'Token.Var; flags = parse_flags []; e = parse_bindexp; stream >] ->
        begin parser
            | [< 'Token.In; e2 = parse_exp >] -> Ast.BindInExp (flags, e, e2)
            | [< >] -> raise @@ Failure "expected 'in' after expression in var binding"
        end stream

and type_of_tid = function
    | "'bool" -> Types.Bool
    | "'int" -> Types.Int
    | "'float" -> Types.Float
    | "'string" -> Types.String
    | t -> raise @@ Parse_error ("invalid type " ^ t);;
