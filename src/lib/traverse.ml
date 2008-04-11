let find_cloneable_transition (state, state_list) =
  let incoming = State.incoming (state, state_list) in
  let transition_count state_from =
    State.transition_count state_from state in
  let sum = List.fold_left (+) 0 (List.map transition_count incoming) in
  let best_transition = ref None in
  let best_transition_count = ref 0 in
  let inspect_transition cur_state =
    let current_transition_count = transition_count cur_state in
    if !best_transition == None ||
        (transition_count cur_state) > !best_transition_count then begin
      best_transition := Some cur_state;
      best_transition_count := current_transition_count
    end
  in begin
    List.iter inspect_transition incoming;
    (* If there is a single transition that makes up the bulk of the incoming
     * transitions, but the other transitions combined produce at least
     * 40% of the total, then clone. *)
    if (float_of_int !best_transition_count) >
        0.4 *. float_of_int (sum - !best_transition_count) then
      !best_transition
    else
      None
  end;;

let rec traverse (state, state_list) input_bit =
  match (try Some (input_bit ()) with End_of_file -> None) with
    None -> ()
  | Some bit ->
    State.incr state bit;
    let next_state = (State.next state bit) in
    let transition_to_clone =
      find_cloneable_transition (next_state, state_list)
    in match transition_to_clone with
      None -> ()
    | Some origin_state ->
      State.clone_out origin_state next_state state_list;
    traverse ((State.next state bit), state_list) input_bit;;

let probability state input_bit =
  let rec probability accu state =
    match (try Some (input_bit ()) with End_of_file -> None) with
      None -> accu
    | Some bit -> 
      (probability
        (State.probability state bit *. accu)
        (State.next state bit))
  in probability 1.0 state;;
