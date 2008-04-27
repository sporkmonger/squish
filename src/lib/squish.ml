open Num

let input_bit = Bit_stream.create_bit_reader (Std.input_chars stdin);;

let buckets bucket_file : (string, State.t * State.t list ref * float) Hashtbl.t =
  try begin
    let in_channel = open_in bucket_file in
    Marshal.from_channel in_channel
  end with Sys_error error -> Hashtbl.create 2

let named_bucket bucket_file bucket_name =
  let buckets = buckets bucket_file in
  Hashtbl.find buckets bucket_name

let create bucket_name bucket_file =
  match bucket_name with
    None -> Printf.printf "No bucket name specified.\n"
  | Some bucket_name -> begin
      let buckets = buckets bucket_file in
      if Hashtbl.mem buckets bucket_name then
        Printf.printf "Bucket already exists.\n"
      else begin
        let state_list = ref [] in
        let initial_state = State.create state_list in
        let bias = 0.0 in
        Hashtbl.replace buckets bucket_name (initial_state, state_list, bias);
        let out_channel = open_out bucket_file in
        Marshal.to_channel out_channel buckets [];
        Printf.printf "Created bucket: '%s'\n" bucket_name
      end
    end
    
let train bucket_name bucket_file =
  match bucket_name with
    None -> Printf.printf "No bucket name specified.\n"
  | Some bucket_name -> begin
      let buckets = buckets bucket_file in
      let (initial_state, state_list, bias) =
        named_bucket bucket_file bucket_name in
      begin
        Traverse.traverse (initial_state, state_list) input_bit;
        Hashtbl.replace buckets bucket_name (initial_state, state_list, bias);
        let out_channel = open_out bucket_file in
        Marshal.to_channel out_channel buckets [];
        Printf.printf "Trained input into '%s'.\n" bucket_name;
        Printf.printf
          "Bucket now contains %d states.\n"
          (List.length !state_list);
        State.print_t initial_state;
        Printf.printf "\n"
      end
    end

let bias bucket_name (new_bias : float) bucket_file =
  match bucket_name with
    None -> Printf.printf "No bucket name specified.\n"
  | Some bucket_name -> begin
      let buckets = buckets bucket_file in
      let (initial_state, state_list, _) =
        named_bucket bucket_file bucket_name in
      begin
        Hashtbl.replace
          buckets bucket_name (initial_state, state_list, new_bias);
        let out_channel = open_out bucket_file in
        Marshal.to_channel out_channel buckets [];
        Printf.printf "Set bias for '%s' to %f.\n" bucket_name new_bias;
      end
    end

let classify bucket_file =
  let input_enum = Std.input_chars stdin in
  let buckets = buckets bucket_file in
  let bucket_length = (Hashtbl.length buckets) in
  let magic_numbers = Hashtbl.create bucket_length in
  let best_bucket = ref None in
  let best_number = ref (0.0 -. 999999999999.0) in
  let sum_numbers = ref 0.0 in
  let calc_numbers bucket_name (initial_state, state_list, bias) =
    begin
      let input_bit =
        Bit_stream.create_bit_reader (Enum.clone input_enum) in
      let magic_number = Traverse.magic_number initial_state input_bit in
      let input_length = Enum.count input_enum in
      let normalized_number = (magic_number /. (float_of_int input_length)) in
      begin
        Hashtbl.replace magic_numbers bucket_name normalized_number;
        sum_numbers := !sum_numbers +. normalized_number;
      end
    end
  in
  let select_best bucket_name magic_number =
    let (initial_state, state_list, bias) =
      Hashtbl.find buckets bucket_name in
    begin
      if magic_number +. bias > !best_number then begin
        if magic_number < !best_number then
          Printf.fprintf stderr "bias overwrote result\n";
        best_bucket := Some bucket_name;
        best_number := magic_number +. bias
      end
    end
  in begin
    Hashtbl.iter calc_numbers buckets;
    Hashtbl.iter select_best magic_numbers;
    match !best_bucket with
      None -> Printf.printf "No bucket matched.  Does a bucket exist?\n"
    | Some bucket_name ->
      Printf.printf "%s\n" bucket_name;
  end;;
