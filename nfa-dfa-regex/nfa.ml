open List
open Utils

(*********)
(* Types *)
(*********)

(* 
  from utils.ml, for your reference

  type ('q, 's) transition = {
    input: 's option; 
    states: 'q * 'q;
  }

  (** NFA type *)
  type ('q, 's) nfa_t = {
    sigma: 's list;
    qs: 'q list;
    q0: 'q;
    fs: 'q list;
    delta: ('q, 's) transition list;
  } 
*)

(****************)
(* Part 1: NFAs *)
(****************)

(*takes in an NFA (nfa), a set of initial states (qs), and symbol options (s)*)
(*
want to return the set of states (list) that NFA could be in after moving from any 
state in qs and making a transition on s
if symbol not in sigma, return empty list
if s is None, make exactly one epsilon transition
*)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  (*check if the symbol is in the alphabet*)
  let check_s symbol = 
    match symbol with
      | None -> true
      | Some c -> List.mem c nfa.sigma in 
  (*if symbol is in alphabet:*)
  if not (check_s s) then [] else
    let rec delta_loop delta_list = 
      match delta_list with
        | [] -> []
        | {input = chara; states = tuple}::xs -> 
          if (chara = s && List.mem (fst tuple) qs) then snd tuple :: delta_loop xs else delta_loop xs
    in List.sort_uniq Stdlib.compare (delta_loop nfa.delta)

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let rec e_loop states visited = let curr_states =
    List.fold_left (fun x state ->
      List.fold_left (fun y delta_list -> match delta_list with
        | {input = None; states = (q1, q2)} -> if not (List.mem q2 visited) && q1 = state then q2 :: y else y
        | _ -> y) x nfa.delta) [] states in
  if curr_states = [] then List.sort_uniq Stdlib.compare visited else e_loop curr_states (visited @ curr_states)
    in e_loop qs qs
      
let accept (nfa: ('q,char) nfa_t) (s: string) : bool = 
  let char_list = explode s in
  let rec loop_string str curr_states = match str with
      | [] -> curr_states
      | x :: xs -> let next = List.fold_left 
        (fun acc state -> 
          let move = move nfa [state] (Some x) in 
          let e = e_closure nfa move in union acc e) [] curr_states
    in loop_string xs next
  in let start_states = e_closure nfa [nfa.q0]
  in let final_states = loop_string char_list start_states in 
  List.exists (fun state -> List.mem state nfa.fs) final_states


(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

(*let nfa_ex = {
    sigma = ['a'];
    qs = [0; 1; 2];
    q0 = 0;
    fs = [2];
    delta = [{input = Some 'a'; states = (0, 1)}; {input = None; states = (1, 2)}]
}*)

(*parameters: nfa and list of states
output: list of state lists
each elem in the returned list is the list you can get to by starting in any state and moving
based on one character from sigma*)
let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = 
  let rec new_states_aux sigma = match sigma with
    |[] -> []
    |x::xs -> let moved = move nfa qs (Some x) in e_closure nfa moved :: new_states_aux xs
  in List.sort_uniq Stdlib.compare (new_states_aux nfa.sigma) 

(*idea: check if move != before move, and create transition if so?*)
(*parameters: NFA and list of states
output: transition list
returns all possible transitions from the input list of states*)
let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  let rec trans_aux sigma = match sigma with
    [] -> []
    | x::xs -> let moved = move nfa qs (Some x) in 
      let e = e_closure nfa moved in {input = (Some x); states = (qs, e)} :: trans_aux xs
  in List.sort_uniq Stdlib.compare(trans_aux nfa.sigma)

(*parameters: NFA and list of states
output: return list of list of starting states or []*)
let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = 
  if List.exists (fun state -> List.mem state nfa.fs) qs then [qs] else []

(*parameters: nfa, dfa to be created (acc), wrk (unvisited states)\
This means that we take an unvisited DFA state from the worklist and add it
to our DFA that we are creating (updating the list of all states, transitions,
and final states appropriately). Our worklist is then updated for the next iteration by
removing the newly processed state. You will want to use the previous three functions as helpers.
They can be used to update the DFA's states, transitions, and final states.*)

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t = 
    match work with
      | state :: xs -> let new_states_list = new_states nfa state in 
        let new_trans = new_trans nfa state in 
        let new_qs = List.sort_uniq Stdlib.compare (state :: dfa.qs) in
        let new_d = List.sort_uniq Stdlib.compare (dfa.delta @ new_trans) in
        let new_final = new_finals nfa state in 
        let new_fs = List.sort_uniq Stdlib.compare (union dfa.fs new_final) in 
        let new_work = List.sort_uniq Stdlib.compare (xs @ List.filter (fun state -> not (List.mem state dfa.qs)) new_states_list) in
        nfa_to_dfa_step nfa {dfa with qs = new_qs; delta = new_d; fs = new_fs} new_work
      |[] -> dfa

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let dfa = {sigma = nfa.sigma; qs = [e_closure nfa [nfa.q0]]; q0 = e_closure nfa [nfa.q0]; fs = []; delta = []} in 
    nfa_to_dfa_step nfa dfa [e_closure nfa [nfa.q0]]
