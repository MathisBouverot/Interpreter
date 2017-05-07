

type token =
    (* modules *)
    | Module | End
    (* conditionals *)
    | If | Then | Else 
    (* message passing *)
    | Receive | After
    (* variable binding *)
    | Var | In | And | Rec | Lazy
    (* functions *)
    | Lambda
    (* identifier *)
    | Ident of string
    | MIdent of string
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

