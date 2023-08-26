// @JackDragoon: 
// This was made & meant to be computed on-chain 
// for immutable test result record.
// 
type storage = (address, int)big_map
//
// STORAGE : 
// write ligo expression on deploy :
//
// ( Big_map.empty : (address, int)big_map ) 
// 
type answer = nat * nat
type answer_sheet = answer list

// empty records
let initial_storage : storage = Big_map.empty

let score_table = Map.literal [
  (1n, {yes = 98; no = 0});
  (2n, {yes = 102; no = 0});
  
  (3n, {yes = 97; no = 0});
  (4n, {yes = 103; no = 0});
  
  (5n, {yes = 105; no = 0});
  (6n, {yes = 95; no = 0});
  
  (7n, {yes = 92; no = 0});
  (8n, {yes = 0; no = 108});

  (9n, {yes = 98; no = 0});
  (10n, {yes = 102; no = 0});
]


let compute_social_score (sheet : answer_sheet ) :  int = 
  let compute_score (question, choice : nat * nat) : int = 
    let record = match Map.find_opt question score_table with
      | Some r -> r
      | None -> failwith "something wrong"
    in if choice = 0n then record.yes else record.no
  in
  let mapping = List.map compute_score sheet in
  let score = List.fold_left (fun(acc, n) -> acc + n ) 0 mapping in
  score


//* entry * //
let main (sheet,_ : answer_sheet * storage) : operation list * storage = 
  // compute new score along new wallet:
  let new_score = compute_social_score sheet in
  // assoc that wallet with new score :
  let current_address = Tezos.get_source() in
  let updated_storage = match Big_map.find_opt current_address initial_storage with 
  | Some _ -> Big_map.update (current_address) (Some(new_score)) initial_storage
  | None -> Big_map.add (current_address) (new_score) initial_storage
  in [], updated_storage
