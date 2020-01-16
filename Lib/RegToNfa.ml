(** Top-level program for building DOT representations of NFAs from regexes *)
open Nfa
open  Int32
open Set

let die fmt = Printf.kprintf (fun s -> prerr_endline s; exit 1) fmt

let typ = ref "nfa"

let spec = 
  [("-type",
    Arg.Symbol (["nfa"; "dfa"; "dfa-minimized"],
                (:=) typ),
    "Output type")]

let usage = "re-nfa [options] regex"

let parse_re r = 
  try Regex.parse r
  with Regex.Parse_error s -> die "Invalid regular expression: %S\n" s


let antichain lhs rhs: (bool*int*float) =

   let startTimeStamp = Sys.time() in

   let nfaA = Regex.compile (parse_re lhs) in
   let nfaB = Regex.compile (parse_re rhs) in
   let (a, b) = Nfa.antichain_in nfaA nfaB [] in

   let endTime0 = Sys.time() in 
   (a, b, (endTime0 -. startTimeStamp)*.float_of_int 1000)

   (*
   let digraphA = Nfa_dot.digraph_of_nfa nfaA in
   let digraphB = Nfa_dot.digraph_of_nfa nfaB in
   Format.printf "%a@." Nfa_dot.format_digraph digraphA;
   Format.printf "%a@." Nfa_dot.format_digraph digraphB;
   *)

  
   ;;

let getStates r:int = 
   let nfaA = Regex.compile (parse_re r) in
   Nfa.getTotalStates nfaA
   ;;
   

   
  (*
  let r = ref None in
  let collect s =
    match !r with None -> r := Some s
                | Some _ -> ()
  in
  Arg.parse spec collect usage;
  match !r, !typ with
  | None, _ -> ()
  | Some r, "nfa" -> 
     let nfa = Regex.compile (parse_re r) in
     let digraph = Nfa_dot.digraph_of_nfa nfa in
     Format.printf "%a@." Nfa_dot.format_digraph digraph;
  | Some r, "dfa" ->
     let nfa = Regex.compile (parse_re r) in
     let dfa = Dfa.determinize nfa in
     let digraph = Nfa_dot.digraph_of_nfa (Dfa.inject dfa) in
     Format.printf "%a@." Nfa_dot.format_digraph digraph;
  | Some r, "dfa-minimized" ->
     let nfa = Regex.compile (parse_re r) in
     let dfa = Dfa.minimize (Dfa.determinize nfa) in
     let digraph = Nfa_dot.digraph_of_nfa (Dfa.inject dfa) in
     Format.printf "%a@." Nfa_dot.format_digraph digraph;
  | _ -> Arg.usage spec usage
*)