
open Ast


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


(* transform all the MultiClosure into Closure *)
and unfold_funs = function
    | Bool _ | Int _ | Float _ | String _ | Var _ as exp -> exp
    | Tuple exps ->
        Tuple (List.map unfold_funs exps)
    | Cond (pred, e1, e2) ->
        let pred' = unfold_funs pred in
        let e1' = unfold_funs e1 in
        let e2' = unfold_funs e2 in
            Cond (pred', e1', e2')
    | BindIn (flags, bind_exp, e2) ->
        let Bind (id, e1) = bind_exp in
        let e1' = unfold_funs e1 in
        let bind_exp' = Bind (id, e1') in
        let e2' = unfold_funs e2 in
            BindIn (flags, bind_exp', e2')
    | TypeIn (type_bind_exp, e2) ->
        let e2' = unfold_funs e2 in
            TypeIn (type_bind_exp, e2')
    | App (e1, e2) ->
        let e1' = unfold_funs e1 in
        let e2' = unfold_funs e2 in
            App (e1', e2')
    | Seq exps ->
        let exps' = List.map unfold_funs exps in
            Seq exps'
    | Closure (id, t1, e, t2, fv) ->
        let e' = unfold_funs e in
            Closure (id, t1, e', t2, fv)
    | BinOp (op, e1, e2) -> BinOp (op, unfold_funs e1, unfold_funs e2)
    | MultiClosure ([], [], _, _) ->
        raise @@ Failure "a function must have at least one argument"
    | MultiClosure ([], _, _, _) | MultiClosure (_, [], _, _) ->
        raise @@ Failure "unfold_funs"
    | MultiClosure (id1 :: [], t1 :: [], e, t2) ->
        unfold_funs @@ Closure (id1, t1, e, t2, [])
    | MultiClosure (id1 :: ids, t1 :: types, e, t2) ->
        let e' = unfold_funs @@ MultiClosure (ids, types, e, t1) in
            Closure (id1, t1, e', t2, [])


(* returns the set of free variable of the expression
   (for populating further up the expression tree)
   and the modified expresion *)
and compute_fv = function
    | Bool _ | Int _ | Float _ | String _ as exp ->
        (exp, VarSet.empty)
    | Var var -> (Var var, VarSet.singleton var)
    | Tuple exps ->
        let exps', exps_fv = List.split @@ List.map compute_fv exps in
        let all_fv = List.fold_left VarSet.union VarSet.empty exps_fv in
            (Tuple exps', all_fv)
    | BinOp (op, e1, e2) ->
        let e1', e1_fv = compute_fv e1 in
        let e2', e2_fv = compute_fv e2 in
            BinOp (op, e1', e2'), VarSet.union e1_fv e2_fv
    | Closure (arg, t1, e, t2, _) ->
        let e', e_fv = compute_fv e in
        let f_fv = VarSet.diff e_fv (VarSet.singleton arg) in
            (Closure(arg, t1, e', t2, VarSet.elements f_fv), f_fv)
    | MultiClosure _ ->
        raise @@ Failure "compute_fv : all MultiClosure must be eliminated before this pass"
    | App (e1, e2) ->
        let e1', e1_fv = compute_fv e1 in
        let e2', e2_fv = compute_fv e2 in
            (App (e1', e2'), VarSet.union e1_fv e2_fv)
    | BindIn (flags, bind_exp, e) ->
        let bind_exp', bind_exp_fv = compute_bind_exp_fv bind_exp in
        let bind_exp_bv = VarSet.singleton @@ (fun (Bind (id, _)) -> id) bind_exp in
        let e', e_fv = compute_fv e in
        (* free variables are the ones free in bind_exp plus the ones free in e,
        minus the one being bound *)
        let all_fv = VarSet.union (VarSet.diff e_fv bind_exp_bv) bind_exp_fv in
            (BindIn (flags, bind_exp', e'), all_fv)
    | TypeIn (type_bind_exp, e) ->
        let e', e_fv = compute_fv e in
            (TypeIn (type_bind_exp, e'), e_fv)
    | Cond (pred, e1, e2) ->
        let pred', pred_fv = compute_fv pred in
        let e1', e1_fv = compute_fv e1 in
        let e2', e2_fv = compute_fv e2 in
            (Cond (pred', e1', e2'), VarSet.union pred_fv (VarSet.union e1_fv e2_fv))
    | Seq exps ->
        let exps', exps_fv = List.split @@ List.map compute_fv exps in
        let all_fv = List.fold_left VarSet.union VarSet.empty exps_fv in
            (Seq exps', all_fv)

and compute_bind_exp_fv (Bind (id, e)) =
    let e', e_fv = compute_fv e in
    let fv = VarSet.diff e_fv (VarSet.singleton id) in
        (Bind (id, e'), fv)
