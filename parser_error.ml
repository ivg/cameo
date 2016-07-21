open Lexing

exception T of (string * position * position)

let throw my_unique_msg =
  let check_pos f = try f () with _ -> dummy_pos in
  Printexc.(print_raw_backtrace stderr (get_raw_backtrace ()));
  let sp = check_pos Parsing.symbol_start_pos in
  let ep = check_pos Parsing.symbol_end_pos  in
  raise (T (my_unique_msg,sp,ep))

let fileposition err_s err_e =
  Printf.sprintf
    "\nFile \"%s\", line %d, at character %d-%d\n"
    err_s.pos_fname err_s.pos_lnum err_s.pos_cnum err_e.pos_cnum

let string_of_exn line (msg,err_s,err_e) =
  let b = Buffer.create 42 in
  if err_s.pos_fname <> "" then
    Buffer.add_string b (fileposition err_s err_e);
  Buffer.add_string b
    (Printf.sprintf "Parse error: %s\n%s\n" msg line);
  let start = max 0 (err_s.pos_cnum - err_s.pos_bol)  in
  for i=1 to start  do
    Buffer.add_char b ' '
  done;
  let diff = max 1 (err_e.pos_cnum - err_s.pos_cnum) in
  for i=1 to diff do
    Buffer.add_char b '^'
  done;
  Buffer.contents b

let extract_line err =
  let line = ref "" in
  try
    let ic = open_in err.pos_fname in
    for i=0 to max 0 (err.pos_lnum - 1) do
      line := input_line ic
    done;
    close_in ic;
    !line
  with exn -> !line

let to_string ((msg,err,_) as exn) =
  let line = extract_line err in
  string_of_exn line exn
