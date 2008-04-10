let input_bit = Bit_reader.create_bit_reader stdin;;

let buckets bucket_file =
  (* try begin *)
    let in_channel = open_in bucket_file in
    Marshal.from_channel in_channel
  (* end with Sys_error error -> Hashtbl.create 2 *)
  
let initial_state bucket_file bucket_name =
  let buckets = buckets bucket_file in
  Hashtbl.find buckets bucket_name

let create bucket_name bucket_file =
  match bucket_name with
    None -> Printf.printf "No bucket name specified.\n"
  | Some name -> begin
      let buckets = buckets bucket_file in begin
        Hashtbl.add buckets bucket_name (State.create ());
        try begin
          let out_channel = open_out bucket_file in
          Marshal.to_channel out_channel buckets []
        end with Sys_error error -> ()
      end
    end
    
let train bucket_name bucket_file =
  match bucket_name with
    None -> Printf.printf "No bucket name specified.\n"
  | Some name -> begin
      let initial_state = initial_state bucket_file name in
      Traverse.traverse initial_state input_bit;
      Printf.printf "\nTrained input into '%s'.\n" name
    end;;

let classify bucket_file =
  Printf.printf "\nNot implemented yet.\n";;
