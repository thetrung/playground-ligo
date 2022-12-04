type token_supply = {
    current_stock : nat;
    token_price : tez
}
type token_shop_storage = (nat, token_supply) map
type return = operation list * token_shop_storage

let owner_address : address = ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV" : address) 

let main(token_kind_index, token_shop_storage : nat  * token_shop_storage) : return =
    // get token_kind
    let token_kind : token_supply = 
        match Map.find_opt (token_kind_index) token_shop_storage with
        | Some k -> k
        | None -> (failwith "Unknown kind of token" : token_supply)
    in
    // NOTE : training code sample lacking this 
    // get amount
    let amount = Tezos.get_amount() in
    // not enough tez sent ? 
    let () = if amount <> token_kind.token_price then
        failwith "Sorry, the token you are trying to purchase has a different price"
    in
    // out of stock ?
     let () = if token_kind.current_stock = 0n then
        failwith "Sorry, the token you are trying to purchase is out of stock"
    in
    // decrease token in storage
    let token_shop_storage = Map.update token_kind_index 
        (Some { 
            token_kind with current_stock = 
            abs(token_kind.current_stock - 1n)
        }) token_shop_storage 
    in
    // get contract unit from onwer address
    let receiver : unit contract = 
    match (Tezos.get_contract_opt owner_address) with
    | Some(contract) -> contract 
    | None -> (failwith ("Not a contract") : (unit contract))
    in
    let payout_operation : operation = 
        Tezos.transaction unit amount receiver
    in
    let operations : operation list = 
    [ payout_operation ]
    in 
    (operations, token_shop_storage)

