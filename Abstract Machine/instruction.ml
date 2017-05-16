

type instruction =
    (* push a constant on the stack *)
    | Int of int
    | Float of float
    | Bool of bool
    | String of string
    (* variables *)
    | Bind of string
    | Access of string
    (* closures *)
    | Closure of string * instruction list * string list (* arg * closure code * free variables *)
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
