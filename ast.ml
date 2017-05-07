
type id = string

type exp =
    (* data types *)
    | BoolExp of bool
    | IntExp of int
    | FloatExp of float
    | StrExp of string
    (* variables and binding *)
    | VarExp of id
    | BindInExp of flag list * bind_exp * exp
    (* conditionals *)
    | CondExp of exp * exp * exp
    (* functions *)
    | FunExp of id * Types.type_exp * exp * Types.type_exp * string list (* free vars *)
    | AppExp of exp * exp
    (* expression sequences *)
    | SeqExp of exp list
    (* program *)
    | ProgExp of exp list

and bind_exp = BindExp of id * exp

and flag = Rec | Lazy

and value =
    | BoolVal of bool
    | IntVal of int
    | FloatVal of float
    | StrVal of string
    | BIFVal of (value -> value)
    | FunVal of id * exp * value Env.env
    | ProgVal of value Env.env
