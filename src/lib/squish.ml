let input_bit = Bit_reader.create_bit_reader stdin;;

let buckets bucket_file =
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
        State.print_t initial_state;
        let out_channel = open_out bucket_file in
        Marshal.to_channel out_channel buckets [];
        Printf.printf "\nTrained input into '%s'.\n" bucket_name;
        Printf.printf
          "Bucket now contains %d states."
          (List.length state_list)
      end
    end

let classify bucket_file =
  Printf.printf "\nNot implemented yet.\n";;
