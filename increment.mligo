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

// Test Unit
let test_increment =
    let initial_storage = 10 in
    let (taddr, _, _) = Test.originate main initial_storage 0tez in
    let contr = Test.to_contract(taddr) in
    let _ = Test.transfer_to_contract_exn contr (Increment (32)) 1mutez in
    assert (Test.get_storage(taddr) = initial_storage + 32)  