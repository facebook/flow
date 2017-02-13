let () =
  let name = Sys_username.get_logged_in_username () in
  Printf.printf "%s\n" name
