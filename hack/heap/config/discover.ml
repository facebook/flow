(** This is a dune configurator:
https://jbuilder.readthedocs.io/en/latest/configurator.html *)

module C = Configurator.V1
module P = C.Pkg_config

(* cmake should have prepared some information for us in the env:
  EXTRA_INCLUDE_PATHS
  EXTRA_LIB_PATHS
  EXTRA_NATIVE_LIBRARIES
*)
let query_env s =
  match Sys.getenv s with
  | t -> Some t
  | exception Not_found -> None

let process_env () =
  let open Option in
  let open String in
  let split = split_on_char ' ' in
  let includes = (query_env "EXTRA_INCLUDE_PATHS") >>| split
  and libs = (query_env "EXTRA_LIB_PATHS") >>| split
  and names = (query_env "EXTRA_NATIVE_LIBRARIES") >>| split in
  let includes = includes >>| (List.map (fun s -> "-I" ^ s))
  and libs = libs >>| (List.map (fun s -> "-L" ^ s))
  and names = names >>| (List.map (fun s -> "-l" ^ s)) in
  match includes, libs, names with
  | Some includes, Some libs, Some names ->
    Some (includes, libs @ names)
  | _ , _, _ -> None

(* In case cmake didn't found anything, fall back to pkg-config *)
let query_pkg (c : C.t) (name : string) : P.package_conf =
  let default : P.package_conf =
    { P.libs  =  [ "-l" ^ name ]
    ; P.cflags = []
    }
  in
  match P.get c with
  | None -> default
  | Some pc ->
    Option.value (P.query pc ~package:("lib" ^ name)) ~default
    (* TODO die instead of default here? *)

let concat_confs
  (c1 : P.package_conf)
  (c2 : P.package_conf)
  : P.package_conf =
  P.({ libs =   c1.libs @ c2.libs
  ; cflags = c1.cflags @ c2.cflags
  })

let () =
  C.main ~name:"heap" (fun (c : C.t) ->
      let env_info = process_env () in
      let (cflags, libs) = match env_info with
        | Some (cflags, libs) -> (cflags, libs)
        | None -> (* fallback to pkg-config *)
          let lz4_conf : P.package_conf = query_pkg c "lz4" in
          let squilte3_conf : P.package_conf = query_pkg c "sqlite3" in
          let conf = concat_confs lz4_conf squilte3_conf in
          (conf.P.cflags, conf.P.libs)
      in
    C.Flags.write_sexp "c_flags.sexp" cflags;
    C.Flags.write_sexp "c_library_flags.sexp" libs)
