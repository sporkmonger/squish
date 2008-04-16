(* We want to keep the number of states in the model under control. *)
let min_count_1 = 256;;
let min_count_2 = 256;;
(* this limit prevents seg faults *)
let max_states = 150000;;
(* this limit prevents a single input from adding 100,000 states in one go *)
let max_cloned_states = 500;;

let traverse (state, state_list) input_bit =
  let state_count = ref (List.length !state_list) in
  let new_states = ref 0 in
  let rec traverse (state, state_list) input_bit =
    match (try Some (input_bit ()) with Enum.No_more_elements -> None) with
      None -> ()
    | Some bit ->
      State.incr state bit;
      let next_state = (State.next state bit) in
      let next_total =
        (State.count next_state 0) + (State.count next_state 1) in
      begin
        if !state_count < max_states && !new_states <= max_cloned_states &&
            State.count state bit > min_count_1 &&
            (next_total - (State.count state bit)) > min_count_2 then begin
          let new_state = State.create state_list in
          let ratio = (float_of_int (State.count state bit)) /.
            (float_of_int next_total) in
          begin
            incr state_count;
            incr new_states;
            State.rebind state bit new_state;
            for bit = 0 to 1 do
              begin
                State.rebind new_state bit (State.next next_state bit);
                let new_count = int_of_float (ratio *.
                  (float_of_int (State.count next_state bit))) in
                let next_count =
                  (State.count next_state bit) - new_count in
                begin
                  State.set_count new_state bit new_count;
                  State.set_count next_state bit next_count;
                end
              end
            done
          end
        end;
        let next_state = (State.next state bit) in
        traverse (next_state, state_list) input_bit
      end
  in traverse (state, state_list) input_bit

let magic_number state input_bit : float =
  let rec magic_number (accu : float) state : float =
    begin
      (* Printf.printf "accu p = %0.10000f\n%!" (float_of_num accu); *)
      match (try Some (input_bit ()) with Enum.No_more_elements -> None) with
        None -> accu
      | Some bit -> 
        magic_number
          ((log (State.probability state bit)) +. accu)
          (State.next state bit)
    end
    in magic_number 0.0 state;;
