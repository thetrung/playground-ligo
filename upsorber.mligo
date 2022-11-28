type transfer =
  [@layout:comb]
  { [@annot:from] address_from : address;
    [@annot:to] address_to : address;
    value : nat }

type approve =
  [@layout:comb]
  { spender : address;
    value : nat }

type allowance_key =
  [@layout:comb]
  { owner : address;
    spender : address }

type getAllowance =
  [@layout:comb]
  { request : allowance_key;
    callback : nat contract }

type getBalance =
  [@layout:comb]
  { owner : address;
    callback : nat contract }

type getTotalSupply =
  [@layout:comb]
  { request : unit ;
    callback : nat contract }

type tokens = (address, nat) big_map
type allowances = (allowance_key, nat) big_map

type token_metadata =
[@layout:comb]
{
  token_id : nat;
  token_info : (string, bytes) map;
}



(*additional entry points*)
(*stake entry-point*)
type stakeEntryParam = {amount: nat; duration: nat;} (*amount in $UP and duration in days*)
(*unstake entry-point*)
type unstakeEntryParam = {id: timestamp} (*id (timestamp) of the stake*)



(* additional types for storage*)
type stakeEntry = {normalizedStarted:timestamp; duration:nat; amount:nat;}
type userStakes =  (timestamp,stakeEntry) map
type stakes = (address,userStakes) big_map


type timepoint = {uptimeStaked:nat; upStaked:nat;}
type timeline = (timestamp,timepoint) big_map


type transformParam = {ref: address}
type withdrawParam = {amount:tez}

type setPriceParam = {amount:nat}

(*
HOW DOES MIN/MAY Days Work?
e.g. start with 30 min days , 1001 max days, doubling: 2 years 
0-2: 30 days
2-4: 60 days
4-6: 120 days
6-8: 240 days
8-10: 480 days
10-12: 960 days


=> 12 years of inflation

*)


type storage = {
  tokens : tokens;
  allowances : allowances;
  total_supply : nat;
  token_metadata : (nat, token_metadata) big_map;
  (*additional storage variables*)
  minDays: nat;
  maxDays: nat;
  doubling: nat;
  startDate: timestamp;
  lastDoubledDate: timestamp; (*needs to be equal to start date at launch*)
  stakes: stakes;
  timeline: timeline;
  stakeTooLate: nat;
  (*transform phase*)
  withdraw: address;
  transformDurationDays: nat;
  currentUPPerTez: nat;
}

type parameter =
  | Transfer of transfer
  | Approve of approve
  | GetAllowance of getAllowance
  | GetBalance of getBalance
  | GetTotalSupply of getTotalSupply
  | Stake of stakeEntryParam
  | Unstake of unstakeEntryParam
  | Transform of transformParam
  | Withdraw of withdrawParam
  | SetTransformPrice of setPriceParam



type result = operation list * storage

[@inline]
let positive (n : nat) : nat option =
  if n = 0n
  then (None : nat option)
  else Some n

let transfer (param, storage : transfer * storage) : result =
  let allowances = storage.allowances in
  let tokens = storage.tokens in
  let allowances =
    if Tezos.get_sender() = param.address_from
    then allowances
    else
      let allowance_key = { owner = param.address_from ; spender = Tezos.get_sender() } in
      let authorized_value =
        match Big_map.find_opt allowance_key allowances with
        | Some value -> value
        | None -> 0n in
      let authorized_value =
        match is_nat (authorized_value - param.value) with
        | None -> (failwith "NotEnoughAllowance" : nat)
        | Some authorized_value -> authorized_value in
      Big_map.update allowance_key (positive authorized_value) allowances in
  let tokens =
    let from_balance =
      match Big_map.find_opt param.address_from tokens with
      | Some value -> value
      | None -> 0n in
    let from_balance =
      match is_nat (from_balance - param.value) with
      | None -> (failwith "NotEnoughBalance" : nat)
      | Some from_balance -> from_balance in
    Big_map.update param.address_from (positive from_balance) tokens in
  let tokens =
    let to_balance =
      match Big_map.find_opt param.address_to tokens with
      | Some value -> value
      | None -> 0n in
    let to_balance = to_balance + param.value in
    Big_map.update param.address_to (positive to_balance) tokens in
  (([] : operation list), { storage with tokens = tokens; allowances = allowances })

let approve (param, storage : approve * storage) : result =
  let allowances = storage.allowances in
  let allowance_key = { owner = Tezos.get_sender() ; spender = param.spender } in
  let previous_value =
    match Big_map.find_opt allowance_key allowances with
    | Some value -> value
    | None -> 0n in
  begin
    if previous_value > 0n && param.value > 0n
    then (failwith "UnsafeAllowanceChange")
    else ();
    let allowances = Big_map.update allowance_key (positive param.value) allowances in
    (([] : operation list), { storage with allowances = allowances })
  end


(*views*)
let getAllowance (param, storage : getAllowance * storage) : operation list =
  let value =
    match Big_map.find_opt param.request storage.allowances with
    | Some value -> value
    | None -> 0n in
  [Tezos.transaction value 0mutez param.callback]

let getBalance (param, storage : getBalance * storage) : operation list =
  let value =
    match Big_map.find_opt param.owner storage.tokens with
    | Some value -> value
    | None -> 0n in
  [Tezos.transaction value 0mutez param.callback]

let getTotalSupply (param, storage : getTotalSupply * storage) : operation list =
  let total = storage.total_supply in
  [Tezos.transaction total 0mutez param.callback]




(*FA1.2 modifications*)


(*helpers*)

let sender : address =  Tezos.get_sender ()

let checkUserUPBalance (amountUP,storage : (nat * storage)) : nat =
 let balanceUP =
    match Big_map.find_opt sender storage.tokens with
      | Some value -> value
      | None -> 0n in 
  
    match is_nat (balanceUP - amountUP) with
      | None -> (failwith "Not enough UP" : nat)
      | Some from_balance -> from_balance


let getNormalizedToday (storage : storage) : timestamp =
  storage.startDate + (((Tezos.get_now() - storage.startDate)/86400)*86400)

let updateMinDay (storage : storage) : nat =
  let today : timestamp = getNormalizedToday(storage) in 
  if today-storage.lastDoubledDate > storage.doubling*86400 then storage.minDays*2n else storage.minDays

let getUserStake (stakeId, storage : (timestamp * storage)) : stakeEntry =
  let stakes : userStakes = match Big_map.find_opt sender storage.stakes with
    | Some k -> k
    | None -> Map.empty
  in 
  match Map.find_opt stakeId stakes with 
    | Some k -> k
    | None -> {normalizedStarted=("1970-01-01t00:00:00Z" : timestamp); duration=0n; amount=0n;} (*equivalent of "not found"*)


let getUserStakes (storage : storage) : userStakes = 
  match Big_map.find_opt sender storage.stakes with
    | Some k -> k
    | None -> Map.empty


let addUserStake (id , stake , storage:  (timestamp * stakeEntry * storage)) : stakes =
  let userStakes : userStakes = getUserStakes(storage) in 
  let newUserStakes : userStakes =  Map.update id (Some stake) userStakes in
  Big_map.update sender (Some newUserStakes) storage.stakes


let removeUserStake (id, storage : (timestamp * storage)) : stakes = 
  let userStakes : userStakes = getUserStakes(storage) in  
  let newUserStakes = Map.remove id userStakes in
  Big_map.update sender (Some newUserStakes) storage.stakes




let updateUserUP (amountUP, storage : (nat * storage)) : tokens = 
  Big_map.update sender (Some amountUP) storage.tokens

let increaseUserUP (address,amountUP, storage : (address * nat * storage)) : tokens = 
  let oldUserUp : nat =  
   match Big_map.find_opt address storage.tokens with
      | Some value -> value
      | None -> 0n in 
  let newUserUp : nat = oldUserUp + amountUP in
  Big_map.update address (Some newUserUp) storage.tokens

let getTimeRewardX (amountUP, days : (nat * nat) ) : nat =   (*change to calculate UPTIME, cause otherwise no rounding like 1.5 etc*)
  (amountUP + ((amountUP*days) / 100n))

let rec updateTimeline (stakeStart, duration, amountUP, newTimeline, recIteration : (timestamp * nat * nat * timeline * nat)) : timeline =
  if (recIteration > duration) then newTimeline else
    let timeEntry : timepoint = match Big_map.find_opt stakeStart newTimeline with 
      | Some k -> k
      | None -> {uptimeStaked=0n;upStaked=0n} in
    let uptime : nat = getTimeRewardX(amountUP,recIteration) in (*dont forget: recIteration must start at 1*) (*AND MAKE SEPERATE REWARD FUNCTION*)
    let newTimeEntry : timepoint = {uptimeStaked=timeEntry.uptimeStaked+uptime; upStaked=timeEntry.upStaked+amountUP} in
    let newTimeline : timeline = Big_map.update stakeStart (Some newTimeEntry) newTimeline in
    let stakeStartIncrease : timestamp = stakeStart + 86400 in
    let recIterationIncrease : nat = recIteration + 1n in
    updateTimeline(stakeStartIncrease,duration,amountUP,newTimeline,recIterationIncrease)

let checkStakeFinish (stakeEntry :  stakeEntry) : bool = 
  let finishDate: timestamp = stakeEntry.normalizedStarted + 86400*stakeEntry.duration in 
  if Tezos.get_now() - finishDate > 0 then true else false 


let getStakeEndTime (stakeEntry : stakeEntry) : timestamp = 
   stakeEntry.normalizedStarted + abs(stakeEntry.duration - 1)*86400

let getDailyInflation(storage : storage) : nat =
  (*300/3650000*)
  (storage.total_supply*300n)/3650000n

let checkStakeTooLate(stakeEndTime,storage : (timestamp * storage)) : bool = 
  let unstakeAble : timestamp = stakeEndTime + storage.stakeTooLate*86400 in
  let today : timestamp = getNormalizedToday(storage) in
  if unstakeAble-today < 0 then true else false


let stake (param,storage : stakeEntryParam * storage) : result =
  (*checks*)
  let newUserBalance : nat = checkUserUPBalance(param.amount,storage) in 
  let () = if param.duration < storage.minDays then (failwith "Too short time period") in
  let () = if param.duration > storage.maxDays then (failwith "Too long time period") in
  let normalizedToday : timestamp = getNormalizedToday(storage) in 
  let nowToday : timestamp = Tezos.get_now() in
  let userStake : stakeEntry = getUserStake(nowToday, storage) in
  let () = if userStake.duration <> 0n then (failwith "Please wait a few seconds") in
  (*checks end*)
  let newTokens : tokens = updateUserUP(newUserBalance, storage) in
  let stakeStart = normalizedToday + 86400 in 
  let newStakes : stakes = addUserStake(nowToday,{normalizedStarted=stakeStart; duration=param.duration; amount=param.amount;}, storage) in
  let newTimeline : timeline = updateTimeline(stakeStart,param.duration,param.amount,storage.timeline,1n) in   
  let newMinDays : nat = updateMinDay(storage) in
  let newlastDoubledDate : timestamp = if newMinDays > storage.minDays then normalizedToday else storage.lastDoubledDate in
(([] : operation list), {storage with timeline=newTimeline; stakes=newStakes; tokens=newTokens; minDays = newMinDays; lastDoubledDate = newlastDoubledDate})



let rec calculateStakeReward (userStake,timeline, rewardAcc,recIteration, stakeEndTime, dailyInflationPool : (stakeEntry  * timeline * nat * nat * timestamp * nat )) : nat =
  let daysInStake : nat = abs (userStake.duration - recIteration ) in 
  let currentUserUptime : nat = getTimeRewardX(userStake.amount,daysInStake) in (*dont forget: daysInStake starts at max *)
  let timeEntry : timepoint = match Big_map.find_opt stakeEndTime timeline with 
    | Some k -> k
    | None -> {uptimeStaked=0n;upStaked=0n} in
  let () = if timeEntry.uptimeStaked = 0n then (failwith recIteration) in
  let reward : nat = (currentUserUptime*dailyInflationPool/timeEntry.uptimeStaked) in
  let newReward : nat = rewardAcc + reward in 
  let newRecIteration : nat = recIteration + 1n in 
  let newStakeEndTime : timestamp = stakeEndTime - 86400 in 
  if stakeEndTime = userStake.normalizedStarted then newReward else 
  calculateStakeReward (userStake, timeline, newReward, newRecIteration, newStakeEndTime,dailyInflationPool)




let unstake (param,storage : unstakeEntryParam * storage) : result = 
  (*checks*)
  let userStake : stakeEntry = getUserStake(param.id, storage) in
  let () = if userStake.duration = 0n then (failwith "No stake at this ID") in
  let () = if checkStakeFinish (userStake) = false then (failwith "Stake not finished yet") in 
  let stakeEndTime : timestamp = getStakeEndTime(userStake) in
  let dailyInflationPool : nat = getDailyInflation(storage) in 
  let reward : nat = if checkStakeTooLate (stakeEndTime,storage) = true then 0n else 
    calculateStakeReward(userStake,storage.timeline,0n,0n,stakeEndTime,dailyInflationPool) in
  let currentUserBalance : nat = checkUserUPBalance(0n,storage) in 
  let payout : nat = currentUserBalance + reward + userStake.amount in 
  let newTokens : tokens = updateUserUP(payout, storage) in (*still not finished, update total supply etc*)
  let newStakes : stakes = removeUserStake(param.id, storage) in
  let newTotalSupply : nat = storage.total_supply + reward in
(([] : operation list), {storage with stakes=newStakes; tokens=newTokens; total_supply=newTotalSupply;})



let transform (param , storage : transformParam * storage) : result = 
  let tezAmount = Tezos.get_amount()/1tez in
  let today = getNormalizedToday(storage) in
  let () = if today-storage.startDate >= storage.transformDurationDays*86400 then (failwith "Transformation phase over") in 
  let () = if tezAmount <= 0n then (failwith "Must send some tez") in   
  let () = if sender = param.ref then (failwith "Sender cant be ref") in   
  let reward : nat = tezAmount*storage.currentUPPerTez in  (*100_000_000n*)
  let newTokens1 : tokens = increaseUserUP(sender,reward,storage) in
  let newStorage : storage = {storage with tokens = newTokens1} in
  (*now for ref*)
  let refReward : nat = reward/10n (*10% reward*) in 
  let newTokens2 : tokens = increaseUserUP(param.ref,refReward,newStorage) in
  let newTotalSupply : nat = storage.total_supply + reward + refReward in
(([] : operation list), {storage with tokens=newTokens2; total_supply=newTotalSupply;})


let withdraw (param, storage : withdrawParam * storage) : result = 
  if storage.withdraw <> sender then (failwith "You cant withdraw" : result)
    else
    let receiver : unit contract =
      match (Tezos.get_contract_opt(storage.withdraw) : unit contract option) with
        Some (contract) -> contract
      | None -> (failwith ("Contract not found") : unit contract) in
    let tx : operation = Tezos.transaction unit param.amount receiver in
([tx]: operation list), storage


let setTransformPrice (param , storage : setPriceParam * storage) : result = 
  let () = if storage.withdraw <> sender then (failwith "You cant set price") in
  (([] : operation list), {storage with currentUPPerTez=param.amount;})



(*views *)


[@view] let getUserStakesView ((),storage: unit * storage) : userStakes = 
  getUserStakes(storage)

[@view] let getEstTotalUserReward (stakeId,storage: timestamp * storage) : nat = 
  let userStake : stakeEntry = getUserStake(stakeId, storage) in
  let () = if userStake.duration = 0n then (failwith "No stake at this ID") in
  let stakeEndTime : timestamp = getStakeEndTime(userStake) in
  let dailyInflationPool : nat = getDailyInflation(storage) in
  if checkStakeTooLate(stakeEndTime,storage) = true then 0n else
    calculateStakeReward(userStake,storage.timeline,0n,0n,stakeEndTime,dailyInflationPool)


[@view] let getEstUserRewardUntilNow(stakeId,storage: timestamp * storage) : nat = 
  let userStake : stakeEntry = getUserStake(stakeId, storage) in
  let () = if userStake.duration = 0n then (failwith "No stake at this ID") in
  let yesterday : timestamp = getNormalizedToday(storage)-86400 in 
  let realStakeEndTime : timestamp = getStakeEndTime(userStake) in 
  let dailyInflationPool : nat = getDailyInflation(storage) in
  if yesterday >= realStakeEndTime then
    if checkStakeTooLate(realStakeEndTime,storage) = true then 0n else 
      calculateStakeReward(userStake,storage.timeline,0n,0n,realStakeEndTime,dailyInflationPool) 
    else
    let daysLeft : nat = abs(realStakeEndTime - yesterday)/86400n in  
    if yesterday - userStake.normalizedStarted < 0 then 0n else
      let stakeEndTime : timestamp = yesterday in
      calculateStakeReward(userStake,storage.timeline,0n,daysLeft,stakeEndTime,dailyInflationPool)



[@view] let getEstRewardsNewStake (stakeEntry,storage: stakeEntryParam * storage) : nat =
  let nowToday : timestamp = Tezos.get_now() in
  let simulateStake : result = stake (stakeEntry,storage) in
  let newStorage : storage = simulateStake.1 in
  let dailyInflationPool : nat = getDailyInflation(newStorage) in 
  let userStake : stakeEntry = getUserStake(nowToday, newStorage) in
  let stakeEndTime : timestamp = getStakeEndTime(userStake) in
  calculateStakeReward(userStake,newStorage.timeline,0n,0n,stakeEndTime,dailyInflationPool)




[@view] let getTotalSupplyView ((),storage: unit * storage) : nat = 
  storage.total_supply

[@view] let getMinDays ((),storage: unit * storage) : nat = 
  storage.minDays

[@view] let getMaxDays ((),storage: unit * storage) : nat = 
  storage.maxDays

[@view] let getStartDate ((),storage: unit * storage) : timestamp = 
  storage.startDate

[@view] let getTimelineForDay (day,storage: timestamp * storage) : timepoint = 
  match Big_map.find_opt day storage.timeline with 
    | Some k -> k
    | None -> {uptimeStaked=0n;upStaked=0n} 

[@view] let getLockedUp ((),storage: unit * storage) : nat = 
  let today : timestamp = getNormalizedToday(storage) in 
  match Big_map.find_opt today storage.timeline with 
    | Some k -> k.uptimeStaked
    | None -> 0n

[@view] let getLastDoubledDate ((),storage: unit * storage) : timestamp = 
  storage.lastDoubledDate

[@view] let getUpForOneTez ((),storage: unit * storage) : nat = 
  storage.currentUPPerTez

(*entry*)
let main (param, storage : parameter * storage) : result =
 match param with
    | Transfer param -> transfer (param, storage)
    | Approve param -> approve (param, storage)
    | GetAllowance param -> (getAllowance (param, storage), storage)
    | GetBalance param -> (getBalance (param, storage), storage)
    | GetTotalSupply param -> (getTotalSupply (param, storage), storage)
    (*additional entries*)
    | Stake param -> stake (param, storage)
    | Unstake param -> unstake (param, storage)
    | Transform param -> transform (param, storage)
    | Withdraw param -> withdraw (param, storage)
    | SetTransformPrice param -> setTransformPrice(param,storage)


