

type cell =
    (* unboxed *)
    | Bool of bool
    | Int of int
    (* boxed *)
    | Float of float
    | Closure of string * Instruction.instruction list * cell Env.env
    | Tuple of cell list
    (* used by the abstract machine only *)
    | Env of cell Env.env
    | Code of Instruction.instruction list;;
