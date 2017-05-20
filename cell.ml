

type cell =
    (* unboxed *)
    | Triv
    | Bool of bool
    | Int of int
    (* boxed *)
    | Float of float
    | Closure of Instruction.instruction list * cell list
    | Tuple of cell list
    (* used by the abstract machine only *)
    | Mem of cell Memory.memory
    | Code of Instruction.instruction list;;
