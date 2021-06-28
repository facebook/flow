


(* let filename = "react.development.js" *)
let filename = "./flow_parser_dot_js.bc.js"

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

;; assert (Sys.file_exists filename)

let content = read_whole_file filename
let start = Unix.gettimeofday ()


let (_,errors) = Parser_flow.program_file ~fail:true content None 


;; assert (errors = [])

;; Format.eprintf "consumed %f@." (Unix.gettimeofday () -. start)