


let make_int_builtin f =
  Ast.BIFVal (
    fun v1 ->
      Ast.BIFVal (
        fun v2 ->
          match v1, v2 with
            | Ast.IntVal i1, Ast.IntVal i2 -> Ast.IntVal (f i1 i2)
            | _ -> raise (Failure "eval_exp1")
      )
  );;

let make_float_builtin f =
  Ast.BIFVal (
    fun v1 ->
      Ast.BIFVal (
        fun v2 ->
          match v1, v2 with
            | Ast.FloatVal f1, Ast.FloatVal f2 -> Ast.FloatVal (f f1 f2)
            | _ -> raise (Failure "eval_exp2")
      )
  );;

let make_ibool_builtin f =
  Ast.BIFVal (
    fun v1 ->
      Ast.BIFVal (
        fun v2 ->
          match v1, v2 with
            | Ast.IntVal b1, Ast.IntVal b2 -> Ast.BoolVal (f b1 b2)
            | _ -> raise (Failure "eval_exp3")
      )
  );;

let make_fbool_builtin f =
  Ast.BIFVal (
    fun v1 ->
      Ast.BIFVal (
        fun v2 ->
          match v1, v2 with
            | Ast.FloatVal b1, Ast.FloatVal b2 -> Ast.BoolVal (f b1 b2)
            | _ -> raise (Failure "eval_exp3")
      )
  );;

let make_bool_builtin f =
  Ast.BIFVal (
    fun v1 ->
      Ast.BIFVal (
        fun v2 ->
          match v1, v2 with
            | Ast.BoolVal b1, Ast.BoolVal b2 -> Ast.BoolVal (f b1 b2)
            | _ -> raise (Failure "eval_exp3")
      )
  );;

let add_int = make_int_builtin (+);;
let add_float = make_float_builtin (+.);;
let sub_int = make_int_builtin (-);;
let sub_float = make_float_builtin (-.);;
let mul_int = make_int_builtin ( * );;
let mul_float = make_float_builtin ( *. );;
let div_int = make_int_builtin (/);;
let div_float = make_float_builtin (/.);;
let mod_int = make_int_builtin (mod);;

let greater = make_ibool_builtin (>);;
let greater_equal = make_ibool_builtin (>=);;
let equal = make_ibool_builtin (=);;
let not_equal = make_ibool_builtin (<>);;
let lesser_equal = make_ibool_builtin (<=);;
let lesser = make_ibool_builtin (<);;

let and_bool = make_bool_builtin (&&);;
let or_bool = make_bool_builtin (||);;
let not_bool = 
  Ast.BIFVal (
    fun v1 ->
      match v1 with
        | Ast.BoolVal true -> Ast.BoolVal false
        | Ast.BoolVal false -> Ast.BoolVal true
        | _ -> raise (Failure "builtin not_bool")
  );;



