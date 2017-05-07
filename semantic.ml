
let rec check_exp type_env = function 
  | Ast.BoolExp _ -> Types.Bool
  | Ast.IntExp _ -> Types.Int
  | Ast.FloatExp _ -> Types.Float
  | Ast.StrExp _ -> Types.String
  | Ast.CondExp (pred, e1, e2) ->
      let pred_type = check_exp type_env pred in
      let e1_type = check_exp type_env e1 in
      let e2_type = check_exp type_env e2 in
        if pred_type <> Types.Bool 
        then raise (Failure "predicate should have type bool")
        else if e1_type <> e2_type 
        then raise (Failure "both branches of a conditional should have same type")
        else e1_type
  (* variables and binding *)
  | Ast.VarExp id -> begin
      try Env.lookup type_env id
      with Not_found ->
        raise @@ Failure ("undeclared variable " ^ id)
    end
  | Ast.BindInExp (bind_exps, e) -> hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
      let e1_type = check_exp type_env e1 in
        check_exp (Env.add type_env id e1_type) e2
  (* functions *)
  | Ast.FunExp (id, t1, e, t2) ->
      let e_type = check_exp (Env.add type_env id t1) e in
        if e_type <> t2 then raise @@ Failure "type mismatch in function body"
        else Types.Function (t1, t2)
  | Ast.AppExp (e1, e2) -> begin
      let e1_type = check_exp type_env e1 in
      let e2_type = check_exp type_env e2 in
        match e1_type with
          | Types.Function (t1, t2) -> 
              if t1 <> e2_type 
              then raise @@ Failure "type mismatch in function application"
              else t2 
          | _ -> raise @@ Failure "application of an expression must be a function"
    end
  (* expression sequences *)
  | Ast.SeqExp [] -> Types.Unit
  | Ast.SeqExp (e :: []) -> 
      check_exp type_env e
  | Ast.SeqExp (e1 :: exps) -> 
      let e1_type = check_exp type_env e1 in
        if e1_type <> Types.Unit 
        then raise @@ Failure ("expressions in a sequence expression "
                               ^ "should have type unit (except the last one)")
        else check_exp type_env (Ast.SeqExp exps)
  | Ast.ModResExp (mid, id) -> begin
      let mod_type = 
        try 
          Env.lookup type_env mid 
        with Not_found ->
          raise @@ Failure ("undeclared module " ^ mid) 
      in
        match mod_type with
          | Types.Module mod_env -> begin
              try Env.lookup mod_env id 
              with Not_found ->
                raise @@ Failure ("undeclared identifier " ^ id)
            end
          | _ -> raise @@ Failure "error cheking module resolution"
    end
  | Ast.ModuleExp _ -> raise @@ Failure "modules can't be defined in expressions"
  | Ast.ProgExp _ -> raise @@ Failure "weird"


(* check a single bind_exp and 
   return a (id, Types.type_expr) pair *)
and check_bind_exp type_env = function
  | Ast.BindExp (id, flags, e) -> 
      let e_type =
        (* if e is a recursive function *)
        if List.mem flags Ast.Rec then
          match e with
            | Ast.FunExp (_, t1, sub_e, t2) -> 
                check_exp (Env.add type_env id (Types.Function(t1, t2))) sub_e
            | _ -> raise @@ Failure "only functions may be recursive"
        else 
          check_exp env e
      in
        (id, e_type)

(* check a module and return the list of the types 
   of all the declarations it contains *)
and check_module type_env mod_env = function
  | Ast.ModuleExp (mid, e1 :: exps) -> begin
      match e1 with
        | Ast.BindExp (id, e2) -> 
            let e2_type = check_exp type_env e2 in
              check_module 
                (Env.add type_env id e2_type) 
                (Env.add mod_env id e2_type) 
                (Ast.ModuleExp (mid, exps))
        | _ -> raise @@ Failure "a module should only contain variable declarations"
    end
  | Ast.ModuleExp (_, []) -> Types.Module mod_env
  | _ -> raise @@ Failure "a program must contain only module declarations"


(* check a program, i.e. a list of modules *)
and check_prog ?(type_env = global_env) = function
  | Ast.ProgExp (Ast.ModuleExp (mid, defs) as modexp :: prog) ->
      (* check the first module *)
      let modexp_type = check_module type_env Env.empty modexp in
        (* check rest of the program *)
        check_prog 
          ~type_env:(Env.add type_env mid modexp_type) 
          (Ast.ProgExp prog)
  | Ast.ProgExp [] -> type_env
  | _ -> raise @@ Failure "a program should only contain module definitions" 


(* types of the builtin functions *)
and global_env = 
  ("!", Types.Function(Types.Bool, Types.Bool)) ::
  List.fold_left 
    (fun env (opname, t1, t2, t3) -> 
       Env.add env opname (Types.Function(t1, Types.Function (t2, t3))))
    Env.empty
    [(* int arithmetics *)
      ("+", Types.Int, Types.Int, Types.Int);
      ("-", Types.Int, Types.Int, Types.Int);
      ("*", Types.Int, Types.Int, Types.Int);
      ("/", Types.Int, Types.Int, Types.Int);
      ("%", Types.Int, Types.Int, Types.Int);
      (* float arithmetic *)
      ("+.", Types.Float, Types.Float, Types.Float);
      ("-.", Types.Float, Types.Float, Types.Float);
      ("*.", Types.Float, Types.Float, Types.Float);
      ("/.", Types.Float, Types.Float, Types.Float);
      (* int relational *)
      (">", Types.Int, Types.Int, Types.Bool);
      (">=", Types.Int, Types.Int, Types.Bool);
      ("==", Types.Int, Types.Int, Types.Bool);
      ("<>", Types.Int, Types.Int, Types.Bool);
      ("<=", Types.Int, Types.Int, Types.Bool);
      ("<", Types.Int, Types.Int, Types.Bool);
      (* float relationals *)
      (">.", Types.Float, Types.Float, Types.Bool);
      (">=.", Types.Float, Types.Float, Types.Bool);
      ("==.", Types.Float, Types.Float, Types.Bool);
      ("<>.", Types.Float, Types.Float, Types.Bool);
      ("<=.", Types.Float, Types.Float, Types.Bool);
      ("<.", Types.Float, Types.Float, Types.Bool);
      (* boolean builtins *)
      ("&&", Types.Bool, Types.Bool, Types.Bool);
      ("||", Types.Bool, Types.Bool, Types.Bool)];;







