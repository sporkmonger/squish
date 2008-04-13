type t = {
  mutable count_zero : int;
  mutable count_one : int;
  mutable transition_zero : t;
  mutable transition_one : t
}

let create state_list =
  let rec new_state = {
    count_zero = 0;
    count_one = 0;
    transition_zero = new_state;
    transition_one = new_state
  } in
  state_list := new_state :: !state_list; new_state

let rebind state bit transition_state =
  if bit = 0 then
    state.transition_zero <- transition_state
  else
    state.transition_one <- transition_state

let incr state bit =
  if bit = 0 then
    state.count_zero <- state.count_zero + 1
  else
    state.count_one <- state.count_one + 1

let set_count state bit new_count =
  if bit = 0 then
    state.count_zero <- new_count
  else
    state.count_one <- new_count

let count state bit =
  if bit = 0 then state.count_zero else state.count_one

let next state bit =
  if bit = 0 then state.transition_zero else state.transition_one

let probability state bit : float =
  (* c = confidence, where high numbers mean low confidence *)
  let c = 2 in
  let total = state.count_zero + state.count_one in
  if total = 0 then 0.0 else
    (float_of_int ((count state bit) + c)) /. (float_of_int (total + (2 * c)))

let predict state : float =
  let total = state.count_zero + state.count_one in
  if total = 0 then 0.0 else
    (float_of_int state.count_zero) /. (float_of_int total)

(* Returns a list of all states which reference the specified state. *)
let incoming (state, state_list) =
  let rec incoming accu remainder =
    match remainder with
      [] -> accu
    | head :: tail ->
      if head.transition_zero == state || head.transition_one == state then
        incoming (head :: accu) tail
      else
        incoming accu tail
  in incoming [] !state_list

(* Returns the number of times state one has transitioned to state two. *)
let transition_count state_from state_to =
  if state_from.transition_zero == state_to then
    state_from.count_zero
  else if state_from.transition_one == state_to then
    state_from.count_one
  else
    0

let string_of_t state =
  let transition_zero =
    if state.transition_zero == state then
      "<self state>" else "<other state>" in
  let transition_one =
    if state.transition_one == state then
      "<self state>" else "<other state>" in
  Printf.sprintf
    "{count_zero = %d; count_one = %d; \
    transition_zero = %s; transition_one = %s}"
    state.count_zero state.count_one transition_zero transition_one

let print_t state = Format.print_string (string_of_t state);;
