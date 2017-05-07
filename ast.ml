
type id = string;;

type exp =
    (* data types *)
    | BoolExp of bool
    | IntExp of int
    | FloatExp of float
    | StrExp of string
    (* variables and binding *)
    | VarExp of id
    | BindInExp of flag list * bind_exp list * exp 
    (* conditionals *)
    | CondExp of exp * exp * exp
    (* functions *)
    | FunExp of id * Types.type_exp * exp * Types.type_exp * string list list (* free vars *)
    | AppExp of exp * exp
    (* expression sequences *)
    | SeqExp of exp list
    (* modules *)
    | ModuleExp of id * (flag list * bind_exp) list 
    | ModResExp of string * string 
    (* program : a list of modules *)
    | ProgExp of exp list

and bind_exp = BindExp of id * exp

and flag = Rec | Lazy

and value =
    | BoolVal of bool
    | IntVal of int
    | FloatVal of float
    | StrVal of string
    | BIFVal of (value -> value)
    (* id * exp * function's env * recursive_funs *)
    | FunVal of id * exp * value Env.env * string list
    | ModuleVal of value Env.env
    | ProgVal of value Env.env;;
