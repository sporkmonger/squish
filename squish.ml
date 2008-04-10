let input_bit = Bit_reader.create_bit_reader stdin;;
let initial_state bucket_file bucket_name = State.create ();;

let create bucket_name bucket_file =
  match bucket_name with
    None -> Printf.printf "No bucket name specified.\n"
  | Some name -> Printf.printf "\nNot implemented yet.\n";;

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
