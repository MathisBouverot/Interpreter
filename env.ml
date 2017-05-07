
type 'a env = (string * 'a) list
let empty : 'a env = []

let rec lookup (env : 'a env) x =
  match env with
    | (y, value) :: _ when y = x -> value
    | [] -> raise Not_found
    | _ :: env -> lookup env x

let add (env : 'a env) x value = 
  (x, value) :: env

let add_multiple env l =
  List.fold_left 
    (fun env (id, value) -> add env id value)
    l
    env
