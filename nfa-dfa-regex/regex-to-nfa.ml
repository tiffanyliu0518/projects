open List
open Nfa
open Utils

(*********)
(* Types *)
(*********)

(* 
  from utils.ml, for your reference:

  type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t Union(Char 'a', Char 'b') = (a|b)
  | Concat of regexp_t * regexp_t Concat (Char 'a', Char 'b') = (ab)
  | Star of regexp_t Star(Union(Char 'a', Char 'b')) = (a|b)*
*)

(*each time, return the regex expression
go thru character by character 
union is or (two branches)
concat is have to go thru both
star is wrapped in epsilon and
nfas have: sigma, qs, q0, fs, delta *)

let regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t = 
  let rec regexp_aux regexp = match regexp with
    | Empty_String -> let state = fresh() in 
      {sigma = []; qs = [state]; q0 = state; fs = [state]; delta = []}

    | Char x -> let start_state = fresh() in let end_state = fresh() in 
      {sigma = [x]; qs = [start_state; end_state]; q0 = start_state; fs = [end_state]; delta = [{input = (Some x); states = (start_state, end_state)}]}

    | Concat(reg1, reg2) -> 
        let nfa1 = regexp_aux reg1 in let nfa2 = regexp_aux reg2 in

        let trans = List.fold_left (fun lst x -> {input = None; states = (x, nfa2.q0)}::lst) [] nfa1.fs in

        {sigma = union nfa1.sigma nfa2.sigma; qs = union nfa1.qs nfa2.qs; q0 = nfa1.q0;
         fs = nfa2.fs; delta = union (union nfa1.delta nfa2.delta) trans}
    
    | Union(reg1, reg2) -> 
        let nfa1 = regexp_aux reg1 in let nfa2 = regexp_aux reg2 in
        let start_state = fresh() in let end_state = fresh() in
        
        let trans_start = [{input = None; states = (start_state, nfa1.q0)}; {input = None; states = (start_state, nfa2.q0)}] in
        let trans1 = List.fold_left (fun lst x -> {input = None; states = (x, end_state)}::lst) [] nfa1.fs in 
        let trans2 = List.fold_left (fun lst x -> {input = None; states = (x, end_state)}::lst) [] nfa2.fs in 
        {sigma = union nfa1.sigma nfa2.sigma; qs = union nfa1.qs nfa2.qs; q0 = start_state;
        fs = [end_state]; delta = union nfa1.delta nfa2.delta @ trans_start @ trans1 @ trans2}

    | Star(reg) ->
        let nfa = regexp_aux reg in
        let start_state = fresh() in let end_state = fresh() in 
        let trans = [{input = None; states = (start_state, nfa.q0)}; {input = None; states = (start_state, end_state)};
          {input = None; states = (end_state, start_state)}] @ 
          List.fold_left (fun lst x -> {input = None; states = (x, end_state)}::lst) [] nfa.fs
    in {sigma = nfa.sigma; qs = nfa.qs; q0 = start_state; fs = [end_state]; delta = nfa.delta @ trans}

    in regexp_aux regexp

(* The following functions are useful for testing, we have implemented them for you *)
let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str
