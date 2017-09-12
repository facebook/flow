exception Out_of_retries

let rec mkdtemp ~retries =
  if retries < 0 then
    raise Out_of_retries
  else
    let tmp_dir = Sys_utils.temp_dir_name in
    let tmp_dir = Path.make tmp_dir in
    let name = Random_id.(short_string_with_alphabet alphanumeric_alphabet) in
    let tmp_dir = Path.concat tmp_dir name in
    try
      let () = Unix.mkdir (Path.to_string tmp_dir) 0o740 in
      tmp_dir
    with
    | Unix.Unix_error _ ->
      mkdtemp ~retries:(retries - 1)

let mkdtemp () =
  mkdtemp ~retries:30

let with_tempdir g =
  let dir = mkdtemp () in
  let f = (fun () -> g dir) in
  Utils.try_finally ~f ~finally:(fun () ->
    Sys_utils.rm_dir_tree (Path.to_string dir))
