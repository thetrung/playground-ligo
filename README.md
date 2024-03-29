# playground-ligo
A collection of Tezos Ligo smart contract to play around beside [LIGO IDE](https://ide.ligolang.org/).

### NOTE August, 2023 
As I want to help some projects on Tezos, I still come back to the ecosystem to learn more and improve my understanding. As I get it, my progression is faster now.

### 1. Why playground ?
Because learning a new language ( Ocaml ) and a smart contract rule at the same time could be messy pretty much, especially on Tezos with Ligo, by 4 different styles that I can find on tutorials. But once I get used to it, I figured out that it has a flow of ML family like what I experienced on F# and OCaml, but much worse editor support.

So it could be better if I can, again, saving up a collection of samples during my learning, just like Rust, Lua and JS before. The quality of this collection could be better with time.

### 2. Important
I will update what I noticed here during the learning that is easy to make newbie fall into mistakes of writing smart contract.

I pick CameLigo as my writing style as it seem to be cleanest one but a lot of mistakes waiting here.

- native types :

      100000 as int - integer
      100000n as nat - unsigned integer
      "100000" as string - String
      
      no return type - unit ()
      type V = A | B - variant 
      Some(x) | None - optional 
      
- data types :

      (1,2,3) as tuple 
      [1,2,3] as list
      {1,2,3} as set
      [(1n, "1st")] as map

- function format : 

      let functionA (arg, arg2 : string * int) : returnType = body

- typing : 

      set(items) -> items set
      list(operation) -> operation list 
      map(key, value) -> (key,value) map
    
- variable/const : 


      let a : int = 2
      in a + 1
      // => 3
    
- boolean :

      && 
      || 
      not 
      = 
      <> 
      > 
      < 
      >= 
      <=


- pattern matching & discard similar to Rust :

      type color =
        | RGB   of int * int * int
        | Gray  of int
        | Default

      let int_of_color (c : color) : int =
        match c with
        | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
        | Gray i -> 232 + i
        | Default -> 0

- loop typically :

        let sum (acc,n : int * int) = acc + n
        List.fold_left sum 0 [1,2] 
  
- iter :

      let rec iter (x, y : nat * nat) : nat =
        if y = 0n then x else iter (y, x mod y)

- big_map vs. map :  

      Big maps are lazily deserialized. This means that only the part of the big map that is accessed or updated is read from disk and the gas costs for accessing and updating a big map don't depend on the size of the big map.Regular maps by contrast are fully deserialized before the beginning of the execution of the contract and the gas costs for MEM, GET, and UPDATE are proportional to the logarithm of the size of the map.


- get source address, which trigger the contract ( tend to be user, this avoid KT address ) :

      Tezos.get_source()
    
- get_contract_opt & transaction :

          // get contract unit from onwer address
          let receiver : unit contract = 
          match (Tezos.get_contract_opt address) with
              | Some(contract) -> contract 
              | None -> (failwith ("Not a contract") : (unit contract))
          in
          let payout_operation : operation = 
              Tezos.transaction unit amount receiver
          in
          let operations : operation list = 
              [ payout_operation ]
