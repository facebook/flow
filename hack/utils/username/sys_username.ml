(** When sudo is used to execute a process, effective user ID is root,
 * but user id will still be the original user. *)
let get_real_user_name () =
  let uid = Unix.getuid () in
  if uid = 1 then
    raise Not_found
  else
    let pwd_entry = Unix.getpwuid uid in
    pwd_entry.Unix.pw_name

let get_logged_in_username () =
  let name = try Unix.getlogin () with
    | Unix.Unix_error(Unix.ENOENT, m, _) when m = "getlogin" -> begin
      (** Linux getlogin(3) man page suggests checking LOGNAME. *)
      try Sys.getenv "LOGNAME" with
      | Not_found -> begin
        Sys.getenv "SUDO_USER"
      end
    end
  in
  if name = "root" then
    get_real_user_name ()
  else
    name
