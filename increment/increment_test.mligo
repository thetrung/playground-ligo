#include "increment.mligo"
// Test Unit
// Run : 
// ligo run test increment_test.mligo
let test_increment =
    let initial_storage = 10 in
    let (taddr, _, _) = Test.originate main initial_storage 0tez in
    let contr = Test.to_contract(taddr) in
    let _ = Test.transfer_to_contract_exn contr (Increment (32)) 1mutez in
    assert (Test.get_storage(taddr) = initial_storage + 32)  