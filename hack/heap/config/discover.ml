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
  | t -> Some t
  | exception Not_found -> None

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
  Option.(
    String.(
      let split s =
        if s = "" then
          []
        else
          split_on_char ' ' s
      in
      let includes = query_env "EXTRA_INCLUDE_PATHS" >>| split
      and dirs = query_env "EXTRA_LIB_PATHS" >>| split
      and names = query_env "EXTRA_NATIVE_LIBRARIES" >>| split
      and opaque_opts = query_env "EXTRA_LINK_OPTS" >>| split in
      let includes = includes >>| List.map (fun s -> "-I" ^ abs s)
      and dirs = dirs >>| List.map (fun s -> "-L" ^ abs s)
      and names = names >>| List.map (fun s -> "-l" ^ s) in
      match (includes, dirs, names, opaque_opts) with
      | (Some includes, Some dirs, Some names, Some opts) ->
        Some (includes, dirs @ names @ opts)
      | (_, _, _, _) -> None))

let () =
  C.main ~name:"heap" (fun (_ : C.t) ->
      let env_info = process_env () in
      let (cflags, cldflags) =
        match env_info with
        | Some (cflags, cldflags) -> (cflags, cldflags)
        | None -> ([], [])
      in
      C.Flags.write_sexp "c_flags.sexp" cflags;
      C.Flags.write_sexp "c_library_flags.sexp" cldflags)
