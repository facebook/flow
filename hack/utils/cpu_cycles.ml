(* This API deliberately does not expose a way to reset the counter, so
 * as not to disturb programs that expect to be able to use it themselves.
 *
 * There are a bunch of caveats about what happens if your
 * process is interrupted or if you get scheduled onto a different
 * processor.
 * *)
external cpu_cycles : unit -> int
  = "ocaml_cpu_cycles" "ocaml_cpu_cycles" "noalloc"
