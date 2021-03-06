
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
        let id, value = eval_bind_exp env flags bind_exp in
        let env' = Env.add env id value in
            eval_exp env' e
    | Ast.TypeInExp (_, e) ->
        eval_exp env e
    (* tuples *)
    | Ast.TupleExp exps ->
        let vals = List.map (eval_exp env) exps in
            Ast.TupleVal vals
    (* functions *)
    | Ast.AppExp (e1, e2) -> begin
        let v1 = eval_exp env e1 in
        let v2 = eval_exp env e2 in
            match v1 with
                | Ast.FunVal (id, sub_e1, f_env_ref) ->
                    let f_env' = Env.add !f_env_ref id v2 in
                        eval_exp f_env' sub_e1
                | Ast.BIFVal f ->  f v2
                | _ -> raise @@ Failure "eval_exp : application of an expression must be a function"
    end
    | Ast.FunExp (x, t1, e, t2, fv_ids) ->
        (* create f_env containing all the ids in fv_ids,
        minus the ones not in the current env (the function itself for instance);
        undeclared identifiers should be found by type checker *)
        let fv_id_values =
            List.fold_left
                (fun fv_values id ->
                    try (id, Env.lookup env id) :: fv_values
                    with Not_found -> fv_values)
                []
                fv_ids
        in
        let fv_ids, fv_values = List.split fv_id_values in
        let f_env = List.fold_left2 Env.add  Env.empty fv_ids fv_values in
            Ast.FunVal (x, e, ref f_env)
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


and eval_bind_exp env flags bind_exp =
    let Ast.BindExp (id, e) = bind_exp in
    let v = eval_exp env e in
    match v with
        | Ast.FunVal (x, e, f_env) ->
            let funval =
                if not @@ List.mem Ast.Rec flags
                then Ast.FunVal (x, e, f_env)
                else
                    (* f_env is already a ref *)
                    let rec f_env' = f_env
                    and funval = Ast.FunVal (x, e, f_env') in
                    f_env' := Env.add (!f_env') id funval;
                        funval
            in id, funval
        | v -> id, v

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
