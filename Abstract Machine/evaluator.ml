
open Cellstack;;
open Instruction;;

let rec eval (stack, memory, code) = match code with
    (* constants *)
    | Bool b :: code -> push stack (Cell.Bool b), memory, code
    | Int n :: code -> push stack (Cell.Int n), memory, code
    | Float f :: code -> push stack (Cell.Float f), memory, code
    (* variables *)
    | Access id :: code ->
        let cell = Env.lookup memory id in
            push stack cell, memory, code
    | Bind id :: code ->
        let cell, stack = pop stack in
            stack, Env.add memory id cell, code
    (* closures *)
    | Closure (id, closure_code, fv_ids) :: code ->
        let fv_vals = List.map (Env.lookup memory) fv_ids in
        let closure_env = Env.add_multiple Env.empty (List.combine fv_ids fv_vals) in
        let closure = Cell.Closure (id, closure_code, closure_env) in
            push stack closure, memory, code
    | Apply :: code ->
        let arg, stack = pop stack in
        let Cell.Closure (id, closure_code, closure_env), stack = pop stack in
        let closure_env' = Env.add closure_env id arg in
            push (push stack (Cell.Code code)) (Cell.Env memory), closure_env', closure_code
    | Return :: _ ->
        let cell, stack = pop stack in
        let Cell.Env new_mem, stack = pop stack in
        let Cell.Code new_code, stack = pop stack in
            push stack cell, new_mem, new_code
    (* integer arithmetic *)
    | AddInt :: code ->
        let Cell.Int a, stack = pop stack in
        let Cell.Int b, stack= pop stack in
            push stack (Cell.Int (a + b)), memory, code
    | SubInt :: code ->
        let Cell.Int a, stack = pop stack in
        let Cell.Int b, stack = pop stack in
            push stack (Cell.Int (b - a)), memory, code
    | MulInt :: code ->
        let Cell.Int a, stack = pop stack in
        let Cell.Int b, stack = pop stack in
            push stack (Cell.Int (a * b)), memory, code
    | DivInt :: code ->
        let Cell.Int a, stack = pop stack in
        let Cell.Int b, stack = pop stack in
            push stack (Cell.Int (b / a)), memory, code
    | ModInt :: code ->
        let Cell.Int a, stack = pop stack in
        let Cell.Int b, stack = pop stack in
            push stack (Cell.Int (b mod a)), memory, code
    (* float arithmetic *)
    | AddFloat :: code ->
        let Cell.Float a, stack = pop stack in
        let Cell.Float b, stack = pop stack in
            push stack (Cell.Float (a +. b)), memory, code
    | SubFloat :: code ->
        let Cell.Float a, stack = pop stack in
        let Cell.Float b, stack = pop stack in
            push stack (Cell.Float (b -. a)), memory, code
    | MulFloat :: code ->
        let Cell.Float a, stack = pop stack in
        let Cell.Float b, stack = pop stack in
            push stack (Cell.Float (a *. b)), memory, code
    | DivFloat :: code ->
        let Cell.Float a, stack = pop stack in
        let Cell.Float b, stack = pop stack in
            push stack (Cell.Float (b /. a)), memory, code
    (* boolean logic *)
    | AndBool :: code ->
        let Cell.Bool a, stack = pop stack in
        let Cell.Bool b, stack = pop stack in
            push stack (Cell.Bool (a && b)), memory, code
    | OrBool :: code ->
        let Cell.Bool a, stack = pop stack in
        let Cell.Bool b, stack = pop stack in
            push stack (Cell.Bool (a || b)), memory, code
    | NotBool :: code ->
        let Cell.Bool a, stack = pop stack in
            push stack (Cell.Bool (not a)), memory, code
    | EqBool :: code ->
        let Cell.Bool a, stack = pop stack in
        let Cell.Bool b, stack = pop stack in
        let res = match a, b with
            | true, true -> true
            | false, false -> true
            | _ -> false
        in
            push stack (Cell.Bool res), memory, code
    (* integer comparison *)
    | EqInt:: code ->
        let Cell.Int a, stack = pop stack in
        let Cell.Int b, stack = pop stack in
            push stack (Cell.Bool (a = b)), memory, code
    | LesserInt :: code ->
        let Cell.Int a, stack = pop stack in
        let Cell.Int b, stack = pop stack in
            push stack (Cell.Bool (b < a)), memory, code
    | GreaterInt :: code ->
        let Cell.Int a, stack = pop stack in
        let Cell.Int b, stack = pop stack in
            push stack (Cell.Bool (b > a)), memory, code
    (* float comparison *)
    | EqFloat:: code ->
        let Cell.Float a, stack = pop stack in
        let Cell.Float b, stack = pop stack in
            push stack (Cell.Bool (a = b)), memory, code
    | LesserFloat :: code ->
        let Cell.Float a, stack = pop stack in
        let Cell.Float b, stack = pop stack in
            push stack (Cell.Bool (b < a)), memory, code
    | GreaterFloat :: code ->
        let Cell.Float a, stack = pop stack in
        let Cell.Float b, stack = pop stack in
            push stack (Cell.Bool (b > a)), memory, code
    | [] -> stack, memory, []
