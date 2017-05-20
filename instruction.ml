

type instruction =
    (* push a constant on the stack *)
    | Int of int
    | Float of float
    | Bool of bool
    | String of string
    (* variables *)
    | Bind of int
    | Access of int
    (* allocate storage for n elements in the current memory *)
    | Allocate of int
    (* closures *)
    (* closure code * number of free variables (taken on the stack) *)
    | Closure of  instruction list * int
    (* pop n closures and make them mutually recursive *)
    | MakeRec of int
    | Return
    | Apply
    (* arithmetic :
        Int a; Int b; IntSub ===> Int a - b *)
    (* integer arithmetic *)
    | AddInt
    | SubInt
    | MulInt
    | DivInt
    | ModInt
    (* float arithmetic *)
    | AddFloat
    | SubFloat
    | MulFloat
    | DivFloat
    (* boolean logic *)
    | AndBool
    | OrBool
    | NotBool
    | EqBool
    (* integer comparison *)
    | EqInt
    | LesserInt
    | GreaterInt
    (* float comparison *)
    | EqFloat
    | LesserFloat
    | GreaterFloat;;
