open Num

(* We want to keep the number of states in the model under control. *)
let min_count_1 = 8;;
let min_count_2 = 8;;

let rec traverse (state, state_list) input_bit =
  match (try Some (input_bit ()) with Enum.No_more_elements -> None) with
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
  let max = ref 0x1000000 in
  let min = ref 0 in
  let mid = ref 0 in
  let in_bytes = ref 0 in
  let out_bytes = ref 3 in
  let pout = ref 3 in
  (* This needs to be fixed to deal with larger outputs *)
  let output_buffer = Buffer.create 1 in
  (* let output_bit = Bit_stream.create_bit_writer output_buffer in *)
  let current_state = ref state in
  begin
    (try
      while true do
        begin
          for i = 0 to 7 do
            match (try Some (input_bit ()) with Enum.No_more_elements -> None) with
                None ->
                  min := !max - 1;
                  raise Enum.No_more_elements
              | Some bit ->
                mid := !min +
                  (int_of_float ((float_of_int (!max - !min - 1)) *.
                    (State.predict !current_state)));
                current_state := State.next state bit;
                if (!mid = !min) then incr mid;
                if (!mid = (!max - 1)) then decr mid;
                if bit = 1 then
                  min := !mid
                else
                  max := !mid;
                while ((!max - !min) < 256) do
                  begin
                    if (bit = 1) then decr max;
                    Buffer.add_char output_buffer (char_of_int (!min lsr 16));
                    incr out_bytes;
                    min := (!min lsl 8) land 0xffff00;
                    max := ((!max lsl 8) land 0xffff00);
                    if (!min >= !max) then max := 0x1000000
                  end
                done
          done;
          incr in_bytes;
          if ((!in_bytes land 0xff) = 0) then begin
            if ((!in_bytes land 0xffff) = 0) then
              Printf.fprintf
                stderr
                "compressing... bytes in %d, bytes out %d, ratio %f\n"
                !in_bytes !out_bytes
                ((float_of_int !out_bytes) /. (float_of_int !in_bytes));
            pout := !out_bytes
          end
        end
      done  
    with Enum.No_more_elements ->
      Buffer.add_char output_buffer (char_of_int (!min lsr 16));
      Buffer.add_char output_buffer (char_of_int ((!min lsr 8) land 0xff));
      Buffer.add_char output_buffer (char_of_int (!min land 0x00ff)));
    Buffer.contents output_buffer
  end

let old_compress state input_bit : string =
  let n = 30 in
  let ms_bit = 1 lsl (n - 1) in
  let ms_mask = (1 lsl n) - 1 in
  let low_bound = ref 0 in
  let high_bound = ref ms_mask in
  let mid_point = ref 0 in
  let output_buffer = Buffer.create 1 in
  let output_bit = Bit_stream.create_bit_writer output_buffer in
  begin
    Buffer.clear output_buffer;
    Printf.printf "current buffer contents: '%s'\n" (Buffer.contents output_buffer);
    (try (
      while true do (* this makes me vomit in my mouth a bit *)
        let (p0 : float) = State.probability state 0 in
        let (p1 : float) = State.probability state 1 in
        begin
          mid_point := 
            ((int_of_float (
              (p1 *. (float_of_int !low_bound)) +.
              (p0 *. (float_of_int !high_bound)) +.
              (p0 +. p1 -. 1.0)
            )) / (int_of_float (p0 +. p1)));
          if !mid_point = !low_bound then incr mid_point; 
          mid_point := !mid_point lor 1; (* force rightmost bit to 1 *)
          let (bit : int) = input_bit () in (* on eof, raise exception *)
          if bit = 1 then 
            low_bound := !mid_point (* pick upper part of range *) 
          else 
            high_bound := !mid_point - 1; (* pick lower part of range *)
          while (!low_bound land ms_bit) = (!high_bound land ms_bit) do
            begin 
              output_bit (!low_bound lsr (n - 1)); (* output one bit *) 
              low_bound := (!low_bound lsl 1) land ms_mask; (* remove the bit *) 
              high_bound := ((!high_bound lsl 1) land ms_mask) + 1; 
            end
          done
        end
      done
    ) with Enum.No_more_elements ->
      while (!mid_point != 0) && (!mid_point != ms_bit) do
        begin 
          output_bit (!mid_point lsr (n - 1)); (* output one bit *) 
          mid_point := (!mid_point lsl 1) land ms_mask (* remove the bit *) 
        end 
      done);
    Printf.printf "current buffer contents: '%s'\n" (Buffer.contents output_buffer);
    Buffer.contents output_buffer
  end

(* let probability state input_bit : num =
  let rec probability (accu : num) state : num =
    begin
      (* Printf.printf "accu p = %0.10000f\n%!" (float_of_num accu); *)
      match (try Some (input_bit ()) with Enum.No_more_elements -> None) with
        None -> accu
      | Some bit -> 
        probability
          ((State.probability state bit) */ accu)
          (State.next state bit)
    end
    in probability (num_of_int 1) state;; *)

let probability state input_bit : float =
  let rec probability (accu : float) state : float =
    begin
      (* Printf.printf "accu p = %0.10000f\n%!" (float_of_num accu); *)
      match (try Some (input_bit ()) with Enum.No_more_elements -> None) with
        None -> accu
      | Some bit -> 
        probability
          ((State.probability state bit) *. accu)
          (State.next state bit)
    end
    in probability 1.0 state;;
