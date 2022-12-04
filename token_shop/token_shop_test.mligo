#include "token_shop.mligo"
// Test Unit
// Run : 
// ligo run test token_shop_test.mligo
let test_token_shop =
    let initial_storage : (nat, token_supply) map = 
    Map.literal [ 
        (0n, { current_stock = 20n ; token_price = 2mutez }) ; 
        (1n, { current_stock = 4n ; token_price = 4mutez }) ; 
    ] in
    let (taddr, _, _) = Test.originate main initial_storage 10tez in
    let contr = Test.to_contract(taddr) in
    // asking for 1n stock with 4mutez
    let result = Test.transfer_to_contract_exn contr 1n 4mutez in
    let after_storage = Test.get_storage(taddr) in
    let after_stock = match (Map.find_opt 1n after_storage) with 
    | Some(supply) -> supply.current_stock
    | None -> failwith "No initial supply provided."
    in
    // assuming it was correctly decreased. 
    assert(after_stock = 3n)
    // Ok!