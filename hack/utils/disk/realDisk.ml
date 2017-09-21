include Disk_sig.Types

let cat filename =
  let ic = open_in_bin filename in
  let len = in_channel_length ic in
  let buf = Buffer.create len in
  Buffer.add_channel buf ic len;
  let content = Buffer.contents buf in
  close_in ic;
  content

let is_file_not_exist_error ~file ~err_msg =
  let msg = Printf.sprintf "%s: No such file or directory" file in
  msg = err_msg

let write_file ~file ~contents =
  let chan = try open_out file with
  | Sys_error err_msg when (is_file_not_exist_error ~file ~err_msg) ->
    raise (No_such_file_or_directory file)
  in
  (output_string chan contents; close_out chan)

let rec mkdir_p = function
  | "" -> failwith "Unexpected empty directory, should never happen"
  | d when not (Sys.file_exists d) ->
    mkdir_p (Filename.dirname d);
    Unix.mkdir d 0o770;
  | d when Sys.is_directory d -> ()
  | d -> raise (NotADirectory d)

let is_directory x =
  try Sys.is_directory x with
  | Sys_error _ ->
    false

let file_exists = Sys.file_exists
let getcwd = Sys.getcwd
let chdir = Sys.chdir
let mkdir = Unix.mkdir
let rename old target =
  if not (file_exists old) then
    raise (No_such_file_or_directory old)
  else if not (file_exists (Filename.dirname target)) then
    raise (No_such_file_or_directory (Filename.dirname target))
  else
    try Sys.rename old target with
    | Sys_error s when s = "Directory not empty" ->
      raise (Rename_target_dir_not_empty target)
