open Num

let input_bit = Bit_stream.create_bit_reader (Stream.of_channel stdin);;

let buckets bucket_file : (string, State.t * State.t list ref) Hashtbl.t =
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
        Hashtbl.replace buckets bucket_name (initial_state, state_list);
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
      let (initial_state, state_list) =
        named_bucket bucket_file bucket_name in
      begin
        Traverse.traverse (initial_state, state_list) input_bit;
        Hashtbl.replace buckets bucket_name (initial_state, state_list);
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

let classify bucket_file =
  let read_all in_channel =
    let buf = ref (String.create 1024) in
    let result = ref "" in begin
      while (input in_channel !buf 0 1024) != 0 do
        result := !result ^ !buf
      done;
      !result
    end in
  let input_string = read_all stdin in
  let buckets = buckets bucket_file in
  let best_bucket = ref None in
  let best_probability = ref (num_of_int 0) in
  let sum_probability = ref (num_of_int 0) in
  let calc_probabilities bucket_name (initial_state, state_list) =
    begin
      Printf.printf "Processing %s...\n%!" bucket_name;
    let input_bit =
      Bit_stream.create_bit_reader (Stream.of_string input_string) in
    let probability = Traverse.probability initial_state input_bit in begin
      sum_probability := !sum_probability +/ probability;
      if probability >/ !best_probability then begin
        best_bucket := Some bucket_name;
        best_probability := probability
      end
    end
  end
  in begin
    Hashtbl.iter calc_probabilities buckets;
    match !best_bucket with
      None -> Printf.printf "No bucket matched.  Does a bucket exist?\n"
    | Some bucket_name ->
      Printf.printf "%s\n" bucket_name
  end;;
