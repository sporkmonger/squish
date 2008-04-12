open Num

let min_count_1 = 4;;
let min_count_2 = 4;;

let rec traverse (state, state_list) input_bit =
  match (try Some (input_bit ()) with Stream.Failure -> None) with
    None -> ()
  | Some bit ->
    State.incr state bit;
    let next_state = (State.next state bit) in
    let next_total =
      (State.count next_state 0) + (State.count next_state 1) in
    begin
      if State.count state bit > min_count_1 && 
          (next_total - (State.count state bit)) > min_count_2 then begin
        let new_state = State.create state_list in
        let ratio = (float_of_int (State.count state bit)) /.
          (float_of_int next_total) in
        begin
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

let compress state input_bit : string =
  ""

let probability state input_bit : num =
  let rec probability (accu : num) state : num =
    begin
      (* Printf.printf "accu p = %0.10000f\n%!" (float_of_num accu); *)
      match (try Some (input_bit ()) with Stream.Failure -> None) with
        None -> accu
      | Some bit -> 
        probability
          ((State.probability state bit) */ accu)
          (State.next state bit)
    end
    in probability (num_of_int 1) state;;
