
open Instruction;;


let rec translate_prog exp =
    let code = translate_exp Env.empty [] exp in
    List.rev code

and translate_exp env code = function
    (* basic constants *)
    | Ast.Bool b -> Bool b :: code
    | Ast.Int n -> Int n :: code
    | Ast.Float f -> Float f :: code
    | Ast.String s -> String s :: code
    (* operators *)
    | Ast.BinOp (op, e1, e2) ->
        let code' = translate_exp env code e1 in
        let code'' = translate_exp env code' e2 in
            (op_to_instruction op) :: code''
    (* variables and binding *)
    | Ast.Var var -> Access (Env.lookup env var) :: code
    | Ast.BindIn (flags, Ast.Bind (id, e1), e) ->
        (* translate e1 *)
        let code' = translate_exp env code e1 in
        let id_num =
            try Env.lookup env id
            with Not_found -> Env.size env
        in
        let code'' = Bind (id_num) :: code' in
        (* add id to the environment *)
        let env' = Env.add env id id_num in
        (* translate e *)
            translate_exp env' code'' e
    | Ast.TypeIn (_, e) ->
        translate_exp env code e
    (* tuples *)
    | Ast.Tuple exps -> raise @@ Failure "Tuples : TODO"
        (*let vals = List.map (eval_exp env) exps in
            Ast.TupleVal vals *)
    (* closures *)
    | Ast.App (e1, e2) ->
        let code' = translate_exp env code e1 in
        let code'' = translate_exp env code' e2 in
            Apply :: code''
    | Ast.Closure (x, _, e, _, fv_ids) ->
        let c_env =
            List.fold_left
                (fun env id -> Env.add env id ((Env.size env) + 1))
                Env.empty
                fv_ids
        in
        let c_env = Env.add c_env x 0 in
        let access_fv_code =
            List.fold_left
                (fun code id -> Access (Env.lookup env id) :: code)
                []
                fv_ids
        in
            let c_code = List.rev @@ Return :: (translate_exp c_env [] e) in
                Closure (c_code, List.length fv_ids) :: (List.append access_fv_code code)
    (* expression sequences *)
    (*| Ast.Seq [] -> raise @@ Failure "empty expression sequence"
    | Ast.Seq (e1 :: []) -> eval_exp env e1
    | Ast.Seq (e1 :: e2) ->
        let _ = eval_exp env e1 in
            eval_exp env (Ast.Seq e2)
    (* conditionals *)
    | Ast.Cond (pred, e1, e2) -> begin
        match eval_exp env pred with
            | Ast.BoolVal true -> eval_exp env e1
            | Ast.BoolVal false -> eval_exp env e2
            | _ -> raise @@ Failure "error while evaluating conditionnal expression"
    end
    | Ast.MultiFun _ -> raise @@ Failure "eval_exp"*)


(*and eval_bind_exp env flags bind_exp =
    let Ast.Bind (id, e) = bind_exp in
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
        | v -> id, v*)

and op_to_instruction = function
        (* integer arithmetics *)
        "+" -> AddInt
        | "-" -> SubInt
        | "*" -> MulInt
        | "/" -> DivInt
        (* float arithmetic *)
        | "+." -> AddFloat
        | "-." -> SubFloat
        | "*." -> MulFloat
        | "/." -> DivFloat
        (* integer relationals *)
        | "==" -> EqInt
        | "<" -> LesserInt
        | ">" -> GreaterInt
        (* float relationnals *)
        | "==." -> EqFloat
        | "<." -> LesserFloat
        | ">." -> GreaterFloat
        (* boolean operations *)
        | "||" -> OrBool
        | "&&" -> AndBool
        | "!" -> NotBool;;
