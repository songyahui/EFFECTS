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


let antichain lhs rhs: unit =
   print_string("=========Antichain=========\n");
   let nfaA = Regex.compile (parse_re lhs) in
   let nfaB = Regex.compile (parse_re rhs) in
  (*print_string (string_of_bool (Nfa.accept nfa ['a';'c']));*) 
  (*StateSet.iter (fun a  -> print_string (Int32.to_string a) ) nfa.finals*)
   let (a, b) = Nfa.antichain_in nfaA nfaB [] in 
   let digraph = Nfa_dot.digraph_of_nfa nfaA in
      Format.printf "%a@." Nfa_dot.format_digraph digraph;
   print_string(string_of_bool (a)^"\n"^string_of_int (b)^"\n")
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