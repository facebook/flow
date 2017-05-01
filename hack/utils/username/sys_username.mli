(**
 * Tries to get the logged in username via various ways, consecutively
 * as each one fails.
 *
 * Raises Not_found if unable to get user name.
 *)
val get_logged_in_username : unit -> string
