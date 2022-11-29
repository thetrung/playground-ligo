type action = Test of nat | Default of unit 

let main (action, _s : action * nat) : operation list * nat =
  match action with
  | Test i -> [], i
  | Default -> [], 0n

let test =
  let address = Test.originate_contract (Test.compile_contract main) (Test.eval 0n) 0tez in
  let typed_address = (Test.cast_address address : (action, nat) typed_address) in
  let contract = Test.to_entrypoint "test" typed_address in
  let gas_fees =  Test.transfer_to_contract_exn contract 1n 0tez in
  "GAS=", gas_fees