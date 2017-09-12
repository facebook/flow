let cat filename =
  let ic = open_in_bin filename in
  let len = in_channel_length ic in
  let buf = Buffer.create len in
  Buffer.add_channel buf ic len;
  let content = Buffer.contents buf in
  close_in ic;
  content

let write_file ~file ~contents =
  let chan = open_out file in
  (output_string chan contents; close_out chan)
