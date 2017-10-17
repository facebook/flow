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
    Unix.mkdir d 0o777;
  | d when Sys.is_directory d -> ()
  | d -> raise (NotADirectory d)

let rec rm_dir_tree path =
  try begin
    let stats = Unix.lstat path in
    match stats.Unix.st_kind with
    | Unix.S_DIR ->
      let contents = Sys.readdir path in
      List.iter (fun name ->
          let name = Filename.concat path name in
          rm_dir_tree name)
        (Array.to_list contents) ;
      Unix.rmdir path
    | Unix.S_LNK | Unix.S_REG | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO
    | Unix.S_SOCK ->
      Unix.unlink path
  end with
  (** Path has been deleted out from under us - can ignore it. *)
  | Sys_error(s) when s = Printf.sprintf "%s: No such file or directory" path ->
    ()
  | Unix.Unix_error(Unix.ENOENT, _, _) ->
    ()

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
