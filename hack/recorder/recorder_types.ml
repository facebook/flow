(** A recording is a sequence of these events discovered by the server
 * which resulted in some action by the server.*)
type event =
  (** The state name of the fresh VCS state. *)
  | Fresh_vcs_state of string
  (** Run a typecheck.
   * TODO: This needs a lot more stuff in it, but is not yet implemented.
   * Useful as an event for now for unit testing. *)
  | Typecheck
  | Stop_recording
