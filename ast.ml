
type id = string

type exp =
    (* data types *)
    | BoolExp of bool
    | IntExp of int
    | FloatExp of float
    | StrExp of string
    | TupleExp of exp list
    (* variables and binding *)
    | VarExp of id
    | TypeInExp of type_bind_exp * exp
    | BindInExp of flag list * bind_exp * exp
    (* conditionals *)
    | CondExp of exp * exp * exp
    (* functions *)
    | FunExp of id * Types.type_exp * exp * Types.type_exp * string list (* free vars *)
    | MultiFunExp of id list * Types.type_exp list * exp * Types.type_exp
    | AppExp of exp * exp
    (* expression sequences *)
    | SeqExp of exp list
and bind_exp = BindExp of id * exp
and type_bind_exp = TypeBindExp of id * Types.type_exp
and flag = Rec
and value =
    | BoolVal of bool
    | IntVal of int
    | FloatVal of float
    | StrVal of string
    | TupleVal of value list
    | BIFVal of (value -> value)
    | FunVal of id * exp * value Env.env
