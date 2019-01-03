(** This is a dune configurator:
https://jbuilder.readthedocs.io/en/latest/configurator.html *)

module C = Configurator.V1
module P = C.Pkg_config

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
    let lz4_conf : P.package_conf = query_pkg c "lz4" in
    let squilte3_conf : P.package_conf = query_pkg c "sqlite3" in
    let conf = concat_confs lz4_conf squilte3_conf in

    C.Flags.write_sexp "c_flags.sexp" conf.P.cflags;
    C.Flags.write_sexp "c_library_flags.sexp" conf.P.libs)
