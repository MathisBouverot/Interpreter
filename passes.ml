module VarSet =
    Set.Make(
        struct
            type t = string
            let compare = Pervasives.compare
        end
    )


let rec all_passes exp =
    let exp', _ = compute_fv @@ unfold_funs exp in
        exp'


(* transform all the Ast.MultiFunExp into Ast.FunExp *)
and unfold_funs = function
    | Ast.BoolExp _ | Ast.IntExp _ | Ast.FloatExp _ | Ast.StrExp _ | Ast.VarExp _ as exp -> exp
    | Ast.CondExp (pred, e1, e2) ->
        let pred' = unfold_funs pred in
        let e1' = unfold_funs e1 in
        let e2' = unfold_funs e2 in
            Ast.CondExp (pred', e1', e2')
    | Ast.BindInExp (flags, bind_exp, e2) ->
        let Ast.BindExp (id, e1) = bind_exp in
        let e1' = unfold_funs e1 in
        let bind_exp' = Ast.BindExp (id, e1') in
        let e2' = unfold_funs e2 in
            Ast.BindInExp (flags, bind_exp', e2')
    | Ast.AppExp (e1, e2) ->
        let e1' = unfold_funs e1 in
        let e2' = unfold_funs e2 in
            Ast.AppExp (e1', e2')
    | Ast.SeqExp exps ->
        let exps' = List.map unfold_funs exps in
            Ast.SeqExp exps'
    | Ast.FunExp (id, t1, e, t2, fv) ->
        let e' = unfold_funs e in
            Ast.FunExp (id, t1, e', t2, fv)
    | Ast.MultiFunExp ([], [], _, _) ->
        raise @@ Failure "a function must have at least one argument"
    | Ast.MultiFunExp ([], _, _, _) | Ast.MultiFunExp (_, [], _, _) ->
        raise @@ Failure "unfold_funs"
    | Ast.MultiFunExp (id1 :: [], t1 :: [], e, t2) ->
        unfold_funs @@ Ast.FunExp (id1, t1, e, t2, [])
    | Ast.MultiFunExp (id1 :: ids, t1 :: types, e, t2) ->
        let e' = unfold_funs @@ Ast.MultiFunExp (ids, types, e, t1) in
            Ast.FunExp (id1, t1, e', t2, [])


(* returns the set of free variable of the expression
   (for populating further up the expression tree)
   and the modified expresion *)
and compute_fv = function
    | Ast.BoolExp _ | Ast.IntExp _ | Ast.FloatExp _ | Ast.StrExp _ as exp ->
        (exp, VarSet.empty)
    | Ast.VarExp var -> (Ast.VarExp var, VarSet.singleton var)
    | Ast.FunExp (arg, t1, e, t2, _) ->
        let e', e_fv = compute_fv e in
        let f_fv = VarSet.diff e_fv (VarSet.singleton arg) in
            (Ast.FunExp(arg, t1, e', t2, VarSet.elements f_fv), f_fv)
    | Ast.MultiFunExp _ ->
        raise @@ Failure "compute_fv : all Ast.MultiFunExp must be eliminated before this pass"
    | Ast.AppExp (e1, e2) ->
        let e1', e1_fv = compute_fv e1 in
        let e2', e2_fv = compute_fv e2 in
            (Ast.AppExp (e1', e2'), VarSet.union e1_fv e2_fv)
    | Ast.BindInExp (flags, bind_exp, e) ->
        let bind_exp', bind_exp_fv = compute_bind_exp_fv bind_exp in
        let bind_exp_bv = VarSet.singleton @@ (fun (Ast.BindExp (id, _)) -> id) bind_exp in
        let e', e_fv = compute_fv e in
        (* free variables are the ones free in bind_exp plus the ones free in e,
        minus the one being bound *)
        let all_fv = VarSet.union (VarSet.diff e_fv bind_exp_bv) bind_exp_fv in
            (Ast.BindInExp (flags, bind_exp', e'), all_fv)
    | Ast.CondExp (pred, e1, e2) ->
        let pred', pred_fv = compute_fv pred in
        let e1', e1_fv = compute_fv e1 in
        let e2', e2_fv = compute_fv e2 in
            (Ast.CondExp (pred', e1', e2'), VarSet.union pred_fv (VarSet.union e1_fv e2_fv))
    | Ast.SeqExp exps ->
        let exps', exps_fv = List.split @@ List.map compute_fv exps in
        let all_fv = List.fold_left VarSet.union VarSet.empty exps_fv in
            (Ast.SeqExp exps', all_fv)

and compute_bind_exp_fv (Ast.BindExp (id, e)) =
    let e', e_fv = compute_fv e in
    let fv = VarSet.diff e_fv (VarSet.singleton id) in
        (Ast.BindExp (id, e'), fv)
