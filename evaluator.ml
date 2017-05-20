
open Stack;;
open Instruction;;

let rec reduce (s, m, c) = function
    | 0 -> s, m, c
    | n -> reduce (eval (s, m, c)) (n-1)

and eval (stack, memory, code) = match code with
    (* constants *)
    | Bool b :: code ->
        push (Cell.Bool b) stack;
        stack, memory, code
    | Int n :: code ->
        push (Cell.Int n) stack;
        stack, memory, code
    | Float f :: code ->
        push (Cell.Float f) stack;
        stack, memory, code
    (* variables *)
    | Access i :: code ->
        let cell = Memory.get memory i in
            push cell stack;
            stack, memory, code
    | Bind i :: code ->
        let cell = pop stack in
            Memory.add memory i cell;
            stack, memory, code
    | Allocate n :: code ->
        Memory.allocate memory n Cell.Triv;
        stack, memory, code
    (* closures *)
    | Closure (closure_code, n) :: code ->
        (* function to pop n elements of a stack *)
        let rec pop_n stack ?(acc = []) = function
            | 0 -> acc
            | n -> let cell = pop stack in
                        pop_n stack ~acc:(cell :: acc) (n-1)
        in
            let cells = pop_n stack n in
            let closure = Cell.Closure (closure_code, cells) in
                push closure stack;
                stack, memory, code
    | Apply :: code ->
        let arg = pop stack in
        let Cell.Closure (closure_code,  cells) = pop stack in
        let closure_mem = Memory.create 5 Cell.Triv in
        let i = ref 0 in
            List.iter
                (fun cell -> Memory.add closure_mem !i cell; i := !i + 1)
                (arg :: cells);
            push (Cell.Mem memory) stack;
            push (Cell.Code code) stack;
            stack, closure_mem, closure_code
    | Return :: _ ->
        let cell = pop stack in
        let Cell.Code new_code = pop stack in
        let Cell.Mem new_mem = pop stack in
            push cell stack;
            stack, new_mem, new_code
    (* integer arithmetic *)
    | AddInt :: code ->
        let Cell.Int a = pop stack in
        let Cell.Int b= pop stack in
            push (Cell.Int (a + b)) stack;
            stack, memory, code
    | SubInt :: code ->
        let Cell.Int a = pop stack in
        let Cell.Int b = pop stack in
            push (Cell.Int (b - a)) stack;
            stack, memory, code
    | MulInt :: code ->
        let Cell.Int a = pop stack in
        let Cell.Int b = pop stack in
            push (Cell.Int (a * b)) stack;
            stack, memory, code
    | DivInt :: code ->
        let Cell.Int a = pop stack in
        let Cell.Int b = pop stack in
            push (Cell.Int (b / a)) stack;
            stack, memory, code
    | ModInt :: code ->
        let Cell.Int a = pop stack in
        let Cell.Int b = pop stack in
            push (Cell.Int (b mod a)) stack;
            stack, memory, code
    (* float arithmetic *)
    | AddFloat :: code ->
        let Cell.Float a = pop stack in
        let Cell.Float b = pop stack in
            push (Cell.Float (a +. b)) stack;
            stack, memory, code
    | SubFloat :: code ->
        let Cell.Float a = pop stack in
        let Cell.Float b = pop stack in
            push (Cell.Float (b -. a)) stack;
            stack, memory, code
    | MulFloat :: code ->
        let Cell.Float a = pop stack in
        let Cell.Float b = pop stack in
            push (Cell.Float (a *. b)) stack;
            stack, memory, code
    | DivFloat :: code ->
        let Cell.Float a = pop stack in
        let Cell.Float b = pop stack in
            push (Cell.Float (b /. a)) stack;
            stack, memory, code
    (* boolean logic *)
    | AndBool :: code ->
        let Cell.Bool a = pop stack in
        let Cell.Bool b = pop stack in
            push (Cell.Bool (a && b)) stack;
            stack, memory, code
    | OrBool :: code ->
        let Cell.Bool a = pop stack in
        let Cell.Bool b = pop stack in
            push (Cell.Bool (a || b)) stack;
            stack, memory, code
    | NotBool :: code ->
        let Cell.Bool a = pop stack in
            push (Cell.Bool (not a)) stack;
            stack, memory, code
    | EqBool :: code ->
        let Cell.Bool a = pop stack in
        let Cell.Bool b = pop stack in
        let res = match a, b with
            | true, true -> true
            | false, false -> true
            | _ -> false
        in
            push (Cell.Bool res) stack;
            stack, memory, code
    (* integer comparison *)
    | EqInt:: code ->
        let Cell.Int a = pop stack in
        let Cell.Int b = pop stack in
            push (Cell.Bool (a = b)) stack;
            stack, memory, code
    | LesserInt :: code ->
        let Cell.Int a = pop stack in
        let Cell.Int b = pop stack in
            push (Cell.Bool (b < a)) stack;
            stack, memory, code
    | GreaterInt :: code ->
        let Cell.Int a = pop stack in
        let Cell.Int b = pop stack in
            push (Cell.Bool (b > a)) stack;
            stack, memory, code
    (* float comparison *)
    | EqFloat:: code ->
        let Cell.Float a = pop stack in
        let Cell.Float b = pop stack in
            push (Cell.Bool (a = b)) stack;
            stack, memory, code
    | LesserFloat :: code ->
        let Cell.Float a = pop stack in
        let Cell.Float b = pop stack in
            push (Cell.Bool (b < a)) stack;
            stack, memory, code
    | GreaterFloat :: code ->
        let Cell.Float a = pop stack in
        let Cell.Float b = pop stack in
            push (Cell.Bool (b > a)) stack;
            stack, memory, code
    | [] -> stack, memory, []
