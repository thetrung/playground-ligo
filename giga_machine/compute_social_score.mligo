// @JackDragoon: 
// This was made & meant to be computed on-chain 
// for immutable test result record.
// 
type storage = (address, int) big_map
//
// STORAGE : 
// write ligo expression on deploy :
//
// ( Big_map.empty : (address, int) big_map ) 
// 
type answer = nat * nat
type answer_sheet = answer list

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
  // compute each pair (1n,1n) of question - choice.
  let compute_score (question, choice : nat * nat) : int = 
    // find the question id to match :
    let record = match Map.find_opt question score_table with
      | Some r -> r
      | None -> failwith "wrong question id."
    in if choice < 2n then
    if choice = 0n then record.yes else record.no
    else failwith "wrong answer id."
  in
  // map to all question-choices :
  let mapping = List.map compute_score sheet in
  // sum them all :
  let score = List.fold_left (fun(acc, n) -> acc + n ) 0 mapping in
  score
   
 

//
//* entry * 
// It's required to take/pass current-storage on every update.
//
let main (sheet, current_storage : answer_sheet * storage) : operation list * storage = 
  // validation : don't take more than 10 answers.
  if List.length sheet > 10n then failwith "duplicated or too long answer sheet."
  else
  // compute new score along new wallet:
  let new_score = compute_social_score sheet in
  
  // assoc that wallet with new score :
  let current_address =  Tezos.get_source() in

  //update current storage & return :
  let updated_storage = Big_map.update (current_address) (Some(new_score)) current_storage
  in [], updated_storage
