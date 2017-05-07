
let rec eval_prog exp = eval_exp global_env exp

and eval_exp env = function
    (* basic constants *)
    | Ast.BoolExp b -> Ast.BoolVal b
    | Ast.IntExp n -> Ast.IntVal n
    | Ast.FloatExp f -> Ast.FloatVal f
    | Ast.StrExp s -> Ast.StrVal s
    (* variables and binding *)
    | Ast.VarExp var -> Env.lookup env var
    | Ast.BindInExp (flags, bind_exp, e) ->
        let id, value = eval_bind_exp env bind_exp in
        let env = Env.add env id value in
            eval_exp env e
    (* functions *)
    | Ast.AppExp (e1, e2) -> begin
        let v1 = eval_exp env e1 in
        let v2 = eval_exp env e2 in
            match v1 with
                | Ast.FunVal (id, sub_e1, f_env) ->
                    let f_env' = Env.add f_env id v2 in
                        eval_exp f_env' sub_e1
                | Ast.BIFVal f ->  f v2
                | _ -> raise @@ Failure "eval_expr : application of an expression must be a function"
    end
    | Ast.FunExp (x, t1, e, t2, fv_ids) ->
        let fv_values = List.map (Env.lookup env) fv_ids in
        let f_env = List.fold_left2 Env.add  Env.empty fv_ids fv_values in
            Ast.FunVal (x, e, f_env)
    (* expression sequences *)
    | Ast.SeqExp [] -> raise @@ Failure "empty expression sequence"
    | Ast.SeqExp (e1 :: []) -> eval_exp env e1
    | Ast.SeqExp (e1 :: e2) ->
        let _ = eval_exp env e1 in
            eval_exp env (Ast.SeqExp e2)
    (* conditionals *)
    | Ast.CondExp (pred, e1, e2) -> begin
        match eval_exp env pred with
            | Ast.BoolVal true -> eval_exp env e1
            | Ast.BoolVal false -> eval_exp env e2
            | _ -> raise @@ Failure "error while evaluating conditionnal expression"
    end
    | Ast.MultiFunExp _ -> raise @@ Failure "eval_exp"
    

and eval_bind_exp env bind_exp =
    let Ast.BindExp (id, e) = bind_exp in
    let v = eval_exp env e in
        id, v

and global_env =
    List.fold_left
        (fun env (opname, bif_val) -> Env.add env opname bif_val)
        Env.empty
        [(* arithmetics builtins *)
        ("+", Builtins.add_int);
        ("+.", Builtins.add_float);
        ("-", Builtins.sub_int);
        ("-.", Builtins.sub_float);
        ("*", Builtins.mul_int);
        ("*.", Builtins.mul_float);
        ("/", Builtins.div_int);
        ("/.", Builtins.div_float);
        ("%", Builtins.mod_int);
        (* relational builtins *)
        (">", Builtins.greater);
        (">=", Builtins.greater_equal);
        ("==", Builtins.equal);
        ("<>", Builtins.not_equal);
        ("<=", Builtins.lesser_equal);
        ("<", Builtins.lesser);
        (* boolean builtins *)
        ("!", Builtins.not_bool);
        ("&&", Builtins.and_bool);
        ("||", Builtins.or_bool)]
