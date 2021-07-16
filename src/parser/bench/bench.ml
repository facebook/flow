(* let filename = "react.development.js" *)
let filename = "./flow_parser_dot_js.data"

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

;;
assert (Sys.file_exists filename)

let content = read_whole_file filename

let start = Unix.gettimeofday ()

let parse content = Parser_flow.program_file ~fail:true content None

let () =
  for i = 0 to 30 do
    ignore (parse content)
  done;
  (* let ch = open_out_gen [Open_append; Open_creat] 0o666 "time.txt" in *)
  output_string stdout (Printf.sprintf "%f" (Unix.gettimeofday () -. start))

(* close_out ch *)
