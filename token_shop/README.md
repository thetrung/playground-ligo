# TOKEN SHOP
As a submission for Tacode.dev challenge

### NOTE
I need to fix this part :

        // Tezos.get_amount() instead of: Tezos.amount
        let amount = Tezos.get_amount() in

        // not enough tez sent ? 
        let () = if amount <> token_kind.token_price then
            failwith "Sorry, the token you are trying to purchase has a different price"
        in
        
 ### RUN
 
        ligo run test token_shop_test.mligo
        
