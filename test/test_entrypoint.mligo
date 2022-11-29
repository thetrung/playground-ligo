type action = Test of nat | Default of unit 

let main (action, _s : action * nat) : operation list * nat =
  match action with
  | Test i -> [], i
  | Default _ -> [], 0n

let test =
  (*
   * val originate_contract : michelson_contract -> michelson_program -> tez -> address
   *
   * Originate a contract with initial storage and initial balance.
   *)
  let address = Test.originate_contract (Test.compile_contract main) (Test.eval 0n) 0tez in
  (*
   * val cast_address : address -> ('param,'storage) typed_address
   *
   * This function casts an address to a typed address. 
   * You will need to "annotate the result with the type you expect".
   * 
   * type ('param, 'storage) typed_address
   *
   * A type for an address of a contract with parameter 'param and storage 'storage.
   *
   *)
  let typed_address = (Test.cast_address address : (action, nat) typed_address) in
  (* 
   * val to_entrypoint : string -> ('param, 'storage) typed_address -> 'e contract
   *
   * Get the contract corresponding to an entrypoint of a typed address: the contract 
   * parameter in the result will be the type of the entrypoint, it needs to be annotated, 
   * entrypoint string should omit the prefix "%", but if passed a string starting 
   * with "%", it will be removed (and a warning emitted).
   *)
  let contract = (Test.to_entrypoint "test" typed_address) in
  (* 
   * val transfer_to_contract : 'param contract -> 'param -> tez -> test_exec_result
   *
   * Bake a transaction by sending an amount of tez with a parameter from the current 
   * source to a contract. Returns the amount of gas consumed by the execution of the contract.
   *)
  let gas_fees =  (Test.transfer_to_contract_exn contract 1n 0tez) in
  (*
   * Return computed gas fees
   *)
  "GAS=", gas_fees