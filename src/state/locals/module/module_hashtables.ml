(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* hash table from module names to all known provider files.
   maintained and used by commit_modules and remove_files *)
(** TODO [perf]: investigate whether this takes too much memory **)
let all_providers = ref (Hashtbl.create 0)

let find_in_all_providers_unsafe modulename =
  Hashtbl.find (!all_providers) modulename

module All_providers_mutator: sig
  type t
  val create: Transaction.t -> t
  val add_provider: t -> File_key.t -> Modulename.t -> unit
  val remove_provider: t -> File_key.t -> Modulename.t -> unit
end = struct
  type t = unit

  let create transaction =
    let old_table = Hashtbl.copy !all_providers in

    let commit () =
      Hh_logger.debug "Committing all_providers hashtable";
      Lwt.return_unit
    in
    let rollback () =
      Hh_logger.debug "Rolling back all_providers hashtable";
      all_providers := old_table;
      Lwt.return_unit
    in

    Transaction.add ~commit ~rollback transaction

  (* Note that the module provided by a file is always accessible via its full
     path, so that it may be imported by specifying (a part of) that path in any
     module system. So, e.g., a file whose full path is /foo/bar.js is considered
     to export a module by that name, so that it is always possible to import it
     from any other file in the file system by using a relative path or some other
     file system navigation convention. The Node module system relies on this
     basic setup.

     In addition, a file may or may not export its module by another name: this
     name is typically shorter, but can be used unambiguously throughout the file
     system to import the module, and this access mechanism is somewhat robust to
     moving files around in the file system. Thus, /foo/bar.js may also export its
     module by the name Bar, using a custom module system like Haste, either
     explicitly (by mentioning Bar in the file) or implicitly (following some
     convention), so that it can be imported from any other file in the file
     system by the name Bar. Other combinations are possible: e.g., all files in a
     directory may export their modules via paths relative to a package name, and
     files elsewhere in the file system can import those modules by providing
     paths relative to that package name. So, e.g., /foo/bar.js may export its
     module via the name Foo/bar.js, with the name Foo may be specified in a
     config file under /foo, and other files may still be able to import it by
     that name when the /foo directory is moved to, say, /qux/foo. *)

  let add_provider () f m =
    let provs = try Utils_js.FilenameSet.add f (find_in_all_providers_unsafe m)
      with Not_found -> Utils_js.FilenameSet.singleton f in
    Hashtbl.replace !all_providers m provs

  let remove_provider () f m =
    let provs = try Utils_js.FilenameSet.remove f (find_in_all_providers_unsafe m)
      with Not_found -> failwith (Printf.sprintf
        "can't remove provider %s of %S, not found in all_providers"
        (File_key.to_string f) (Modulename.to_string m))
    in
    Hashtbl.replace !all_providers m provs
end

(* We actually don't need a mutator for module_name_candidates_cache. There are a few reasons why
 * 1. It's really only used for memoization. We never remove or replace anything
 * 2. The code which populates it never changes during the lifetime of a server. So we never
 *    really need to roll anything back ever *)
let module_name_candidates_cache = Hashtbl.create 50
let memoize_with_module_name_candidates_cache ~f name =
  try Hashtbl.find module_name_candidates_cache name
  with Not_found ->
    let result = f name in
    Hashtbl.add module_name_candidates_cache name result;
    result
