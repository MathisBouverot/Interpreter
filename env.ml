
type 'a env = (string * 'a) list * int

let empty : 'a env = [], 0

let rec lookup ((env, size) : 'a env) x =
    match env with
        | [] -> raise Not_found
        | (y, value) :: _ when y = x -> value
        | _ :: env -> lookup (env, size) x

let add ((env, size) : 'a env) x value =
    ((x, value) :: env), size + 1

let add_multiple env l =
    List.fold_left  (fun env (id, value) -> add env id value) l env

let size ((_, size) : 'a env) = size
