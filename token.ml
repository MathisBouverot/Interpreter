

type token =
    (* conditionals *)
    | If | Then | Else
    (* type and variable binding *)
    | Type | Var | In | Rec
    (* functions *)
    | Lambda
    (* identifier *)
    | Ident of string
    | TIdent of string
    (* data types *)
    | Bool of bool
    | Int of int
    | Float of float
    | String of string
    (* Operators *)
    | BinOp of string | UnOp of string
    (* punctuation *)
    | Punct of char
    (* else *)
    | Kwd of char
