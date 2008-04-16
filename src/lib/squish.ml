open Num

let input_bit = Bit_stream.create_bit_reader (Std.input_chars stdin);;

(* let enum_of_string input_string : char Enum.t =
  let i = ref 0 in
  Enum.from (
    fun () ->
      try
        let c = String.get input_string !i in
        begin
          incr i; c
        end
      with Invalid_argument "index out of bounds" ->
        raise Enum.No_more_elements
  ) *)

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
  let input_enum = Std.input_chars stdin in
  let buckets = buckets bucket_file in
  let best_bucket = ref None in
  let best_number = ref (0.0 -. 999999999999.0) in
  let sum_numbers = ref 0.0 in
  let calc_numbers bucket_name (initial_state, state_list) =
    begin
      (* Printf.printf "Processing %s...\n%!" bucket_name;
      State.print_t initial_state;
      Printf.printf "\n"; *)
    let input_bit =
      Bit_stream.create_bit_reader (Enum.clone input_enum) in
    let magic_number = Traverse.magic_number initial_state input_bit in
    begin
      sum_numbers := !sum_numbers +. magic_number;
      if magic_number > !best_number then begin
        best_bucket := Some bucket_name;
        best_number := magic_number
      end;
      (* Printf.printf "bucket: %s, number=%f\n" bucket_name magic_number; *)
    end
  end
  in begin
    Hashtbl.iter calc_numbers buckets;
    match !best_bucket with
      None -> Printf.printf "No bucket matched.  Does a bucket exist?\n"
    | Some bucket_name ->
      Printf.printf "%s\n" bucket_name;
      (* Printf.printf "number=%f\n" !best_number *)
  end;;

(* let classify bucket_file =
  let input_enum = Std.input_chars stdin in
  let buckets = buckets bucket_file in
  let best_bucket = ref None in
  let best_ratio = ref 9999.0 in
  let sum_ratios = ref 0.0 in
  let calc_ratios bucket_name (initial_state, state_list) =
    begin
      Printf.printf "Processing %s...\n%!" bucket_name;
      State.print_t initial_state;
      Printf.printf "\n";
    let input_bit =
      Bit_stream.create_bit_reader (Enum.clone input_enum) in
    let compressed_string = Traverse.compress initial_state input_bit in
    let ratio =
      (float_of_int (String.length compressed_string)) /.
      (float_of_int (Enum.count input_enum)) in
    begin
      sum_ratios := !sum_ratios +. ratio;
      if ratio < !best_ratio then begin
        best_bucket := Some bucket_name;
        best_ratio := ratio
      end;
      Printf.printf "bucket: %s, ratio=%f\n" bucket_name ratio;
      Printf.printf "original length: %d\n" (Enum.count input_enum);
      Printf.printf "compressed length: %d\n" (String.length compressed_string);
      Printf.printf "compressed:\n%s\n" compressed_string
    end
  end
  in begin
    Hashtbl.iter calc_ratios buckets;
    match !best_bucket with
      None -> Printf.printf "No bucket matched.  Does a bucket exist?\n"
    | Some bucket_name ->
      Printf.printf "%s\n" bucket_name;
      Printf.printf "ratio=%f\n" !best_ratio
  end;; *)

(* let classify bucket_file =
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
  let best_probability = ref 0.0 in
  let sum_probability = ref 0.0 in
  let calc_probabilities bucket_name (initial_state, state_list) =
    begin
      Printf.printf "Processing %s...\n%!" bucket_name;
    let input_bit =
      Bit_stream.create_bit_reader (Stream.of_string input_string) in
    let probability = Traverse.probability initial_state input_bit in begin
      sum_probability := !sum_probability +. probability;
      if probability > !best_probability then begin
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
  end;; *)

(* let classify bucket_file =
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
  end;; *)
