type command_t = Help | Create | Train | Classify;;

let command = ref Help;;
let bucket_file = ref (UnixExtras.tilde_expand "~/.squish_buckets");;
let bucket_name = ref None;;

let spec_list = [
  ("--create", Arg.String (fun bucket ->
      command := Create;
      bucket_name := Some bucket),
    "Creates a bucket with the specified name");
  ("--train", Arg.String (fun bucket ->
      command := Train;
      bucket_name := Some bucket),
    "Trains the input into the specified bucket");
  ("--classify", Arg.Unit (fun () ->
      command := Classify),
    "Classifies the input as one of the available buckets")
];;
let usage = "squish <options> [bucketfile]";;
let anon_fun = (fun file -> bucket_file := (UnixExtras.tilde_expand file));;

Arg.parse spec_list anon_fun usage;;

match !command with
  Help -> begin
    Arg.usage spec_list usage;
    exit 0
  end
| Create -> Squish.create !bucket_name !bucket_file
| Train -> Squish.train !bucket_name !bucket_file
| Classify -> Squish.classify !bucket_file;;
