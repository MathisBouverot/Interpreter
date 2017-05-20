
type id = string

type exp =
    (* data types *)
    | Bool of bool
    | Int of int
    | Float of float
    | String of string
    | Tuple of exp list
    (* variables and binding *)
    | Var of id
    | TypeIn of type_bind_exp * exp
    | BindIn of flag list * bind_exp * exp
    (* conditionals *)
    | Cond of exp * exp * exp
    (* closures *)
    | Closure of id * Types.type_exp * exp * Types.type_exp * string list (* free vars *)
    | MultiClosure of id list * Types.type_exp list * exp * Types.type_exp
    | App of exp * exp
    (* operators *)
    | BinOp of string * exp * exp
    (* expression sequences *)
    | Seq of exp list
and bind_exp = Bind of id * exp
and type_bind_exp = TypeBind of id * Types.type_exp
and flag = Rec
(*and value =
    | BoolVal of bool
    | IntVal of int
    | FloatVal of float
    | StrVal of string
    | TupleVal of value list
    | BIFVal of (value -> value)
    | FunVal of id * exp * value Env.env*)
