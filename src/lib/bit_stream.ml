let create_bit_reader (stream : char Stream.t) =
  let buffer = ref 0 in
  let position = ref 0 in
  function () ->
    if !position = 0 then
      buffer := int_of_char (Stream.next stream);
    incr position;
    if !position = 8 then
      position := 0;
    let bit_value = !buffer land 128 in
    buffer := !buffer lsl 1;
    if bit_value = 0 then 0 else 1;;

let create_bit_writer (output_buffer : Buffer.t) =
  let buffer = ref 0 in
  let position = ref 0 in
  function bit ->
    if !position = 0 then begin
      Buffer.add_char output_buffer (char_of_int !buffer);
      buffer := 0
    end;
    incr position;
    if !position = 8 then
      position := 0;
    buffer := !buffer lsr 1;
    buffer := !buffer lor (bit * 128);;
  