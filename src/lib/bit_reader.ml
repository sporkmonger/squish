let create_bit_reader in_channel =
  let buffer = ref 0 in
  let position = ref 0 in
  function () ->
    if !position = 0 then
      buffer := input_byte in_channel;
    incr position;
    if !position = 8 then
      position := 0;
    let bit_value = !buffer land 128 in
    buffer := !buffer lsl 1;
    if bit_value = 0 then 0 else 1;;
