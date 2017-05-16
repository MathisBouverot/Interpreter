

type stack = Cell.cell list

let empty = []

let push stack cell =
    cell :: stack

let pop = function
    | cell :: stack -> cell, stack
    | [] -> raise @@ Failure "pop"
