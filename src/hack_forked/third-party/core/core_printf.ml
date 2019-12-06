include Printf

(** failwith, invalid_arg, and exit accepting printf's format. *)

let failwithf fmt = ksprintf (fun s () -> failwith s) fmt

let invalid_argf fmt = ksprintf (fun s () -> invalid_arg s) fmt

let exitf fmt =
  ksprintf
    (fun s () ->
      Printf.eprintf "%s\n%!" s;
      exit 1)
    fmt
