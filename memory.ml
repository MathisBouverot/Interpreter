

type 'a memory = 'a array ref

let create n (elem : 'a) : 'a memory=
    ref (Array.make n elem)

let add (mem : 'a memory) i (elem : 'a) =
    (!mem).(i) <- elem

let allocate (mem : 'a memory) n elem =
    let length = Array.length !mem in
    let new_mem = Array.make (length + n) elem in
        Array.blit !mem 0 new_mem 0 length;
        mem := new_mem

let get (mem: 'a memory) i =
    (!mem).(i)
