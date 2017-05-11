
type type_exp =
    | Unit
    | Bool
    | Int
    | Float
    | String
    | TypeVar of string
    | Tuple of type_exp list
    | Function of type_exp * type_exp
