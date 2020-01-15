type state = int32
module StateSet = Set.Make(Int32)
module CharMap = Map.Make(Char)
type transitions = StateSet.t CharMap.t

type productState = state * StateSet.t 



type nfa = {
  start : StateSet.t;
  (** the start states *)

  finals: StateSet.t;
  (** the final (or "accept") states *)

  next: state -> transitions;
  (** the transition function, that maps a state and a character to a
      set of states *)
}

let find_states sym nfa m =
  try CharMap.find sym (nfa.next m)
  with Not_found -> StateSet.empty

let flat_map f ss = StateSet.fold (fun s -> StateSet.union (f s)) ss StateSet.empty
let nextss curs (sym:char) nfa = flat_map (find_states sym nfa) curs

(** A simple NFA interpreter. *)
let accept nfa inp =
  (** cur is the set of all the current states -- i.e. those states at
      which we have arrived by examining the input up to this point.
      Since the automaton is non-deterministic, encountering a character
      in a given state can cause transitions to multiple different
      states *)
  let rec step cur = function
    | [] -> StateSet.(not (is_empty (inter cur nfa.finals)))
    | c :: cs -> step (nextss cur c nfa) cs
  in step nfa.start inp


let rec antichain_in (nfaA:nfa) (nfaB:nfa) (processed:(StateSet.t * StateSet.t) list) :(bool*int) = 
  
  (*
  if (List.length processed >100) then (false, 100) 
  else 
  *)
  if StateSet.is_empty nfaA.start then (true, 0) 
  else if StateSet.is_empty nfaB.start then (false, 1) 
  else if List.exists (fun (s, ps) -> (StateSet.subset (nfaA.start) s) && (StateSet.subset ps (nfaB.start))) processed then 
    (true, 1) 
  else 
  let accpLHS = StateSet.(not (is_empty (inter nfaA.start nfaA.finals))) in
  let rejectRHS = StateSet.(is_empty (inter nfaB.start nfaB.finals)) in
  (*let next = StateSet.iter (fun a -> (a, nfaB.start)) nfaA in*)
  if accpLHS  && rejectRHS then (false, 1)
  else 
    let nextA_a = nextss (nfaA.start) 'A' nfaA in 
    let nextB_a = nextss (nfaB.start) 'A' nfaB in 
    let nfaA' = {start = nextA_a; finals = nfaA.finals; next = nfaA.next} in 
    let nfaB' = {start = nextB_a; finals = nfaB.finals; next = nfaB.next} in 
    
    let nextA_b = nextss (nfaA.start) 'B' nfaA in 
    let nextB_b = nextss (nfaB.start) 'B' nfaB in 
    let nfaA'' = {start = nextA_b; finals = nfaA.finals; next = nfaA.next} in 
    let nfaB'' = {start = nextB_b; finals = nfaB.finals; next = nfaB.next} in 

    let (a_trans, state1) = antichain_in nfaA' nfaB' ((nfaA.start, nfaB.start) :: processed) in 
    let (b_trans, state2) = antichain_in nfaA'' nfaB'' ((nfaA.start,nfaB.start) :: processed) in 

    (*print_string(string_of_int (state1) ^ "+" ^ string_of_int(state2)^"\n");*)
    (a_trans && b_trans, state1+state2+1)
  ;; 


let getTotalStates (nfa:nfa) : int = 
  let rec helper cur (acc:StateSet.t) : StateSet.t  =
    let nextA = nextss cur 'A' nfa in
    let nextB = nextss cur 'B' nfa in
    let newAcc = StateSet.union acc (StateSet.union nextA nextB ) in 
    if StateSet.compare newAcc acc == 0 then acc
    else helper (StateSet.union nextA nextB) newAcc
  in 
  let states = helper nfa.start nfa.start in 
  StateSet.cardinal states;;
  
