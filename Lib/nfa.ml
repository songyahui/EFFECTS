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


let rec antichain (nfaA:nfa) (nfaB:nfa) processed :bool = 
  if StateSet.is_empty nfaA.start then true 
  else if List.exists (fun ps -> StateSet.subset ps (nfaB.start)) processed then true 
  else 
  let accpLHS = StateSet.(not (is_empty (inter nfaA.start nfaA.finals))) in
  let rejectRHS = StateSet.(is_empty (inter nfaB.start nfaB.finals)) in
  (*let next = StateSet.iter (fun a -> (a, nfaB.start)) nfaA in*)
  if accpLHS  && rejectRHS then false
  else 
    let nextA_a = nextss (nfaA.start) 'a' nfaA in 
    let nextB_a = nextss (nfaB.start) 'a' nfaB in 
    let nfaA' = {start = nextA_a; finals = nfaA.finals; next = nfaA.next} in 
    let nfaB' = {start = nextB_a; finals = nfaB.finals; next = nfaB.next} in 
    let nextA_b = nextss (nfaA.start) 'b' nfaA in 
    let nextB_b = nextss (nfaB.start) 'b' nfaB in 
    let nfaA'' = {start = nextA_b; finals = nfaA.finals; next = nfaA.next} in 
    let nfaB'' = {start = nextB_b; finals = nfaB.finals; next = nfaB.next} in 

    let a_trans = antichain nfaA' nfaB' (nfaB.start :: processed) in 
    let b_trans = antichain nfaA'' nfaB'' (nfaB.start :: processed) in 

    print_string((string_of_bool a_trans)^" ++ " ^(string_of_bool b_trans) ^"\n");


    a_trans && b_trans
  ;; 
