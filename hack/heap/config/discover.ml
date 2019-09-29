(** This is a dune configurator:
https://jbuilder.readthedocs.io/en/latest/configurator.html *)

module C = Configurator.V1

(* cmake should have prepared some information for us in the env:
  EXTRA_INCLUDE_PATHS
  EXTRA_LIB_PATHS
  EXTRA_NATIVE_LIBRARIES
  EXTRA_LINK_OPTS
*)
let query_env s =
  match Sys.getenv s with
  | "" -> []
  | s -> String.split_on_char ' ' s
  | exception Not_found -> []

let abs =
  let current_dir = Sys.getcwd () in
  (* we are in ./src/heap/config, locate . *)
  let root_dir = Filename.(dirname @@ dirname @@ dirname current_dir) in
  fun s ->
    if Filename.is_relative s then
      Filename.concat root_dir s
    else
      s

let process_env () =
  let includes =
    query_env "EXTRA_INCLUDE_PATHS" |> List.map (fun s -> "-I" ^ abs s)
  in
  let dirs = query_env "EXTRA_LIB_PATHS" |> List.map (fun s -> "-L" ^ abs s) in
  let names =
    query_env "EXTRA_NATIVE_LIBRARIES" |> List.map (fun s -> "-l" ^ s)
  in
  let opaque_opts = query_env "EXTRA_LINK_OPTS" in
  (includes, dirs @ names @ opaque_opts)

let () =
  C.main ~name:"heap" (fun (_ : C.t) ->
      let (cflags, cldflags) = process_env () in
      C.Flags.write_sexp "c_flags.sexp" cflags;
      C.Flags.write_sexp "c_library_flags.sexp" cldflags)
