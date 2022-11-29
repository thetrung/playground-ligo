type storage = int

type parameter = 
    Increment of int
    | Decrement of int
    | Reset

// Two entrypoints

let add (store, delta : storage * int ) = store + delta
let sub (store, delta : storage * int ) = store - delta

(* Main entrypoint that dispatches to entrypoints by SM params *)
let main (action, store : parameter * storage) : operation list * storage =
    [],
    (match action with
        Increment (n) -> add (store, n)
        | Decrement (n) -> sub (store, n)
        | Reset         -> 0
    )
