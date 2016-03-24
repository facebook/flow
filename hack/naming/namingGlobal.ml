(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(** Module "naming" a program.
 *
 * The naming phase consists in several things
 * 1- get all the global names
 * 2- transform all the local names into a unique identifier
 *)
open Core
open Utils
open Naming_heap

module SN = Naming_special_names

(*****************************************************************************)
(* The types *)
(*****************************************************************************)

let canon_key = String.lowercase

module GEnv = struct

  let type_pos name = match TypeIdHeap.get name with
    | Some (p, _k) -> Some p
    | None -> None

  let type_canon_name name = TypeCanonHeap.get (canon_key name)

  let type_info = TypeIdHeap.get

  let fun_pos name = FunPosHeap.get name

  let fun_canon_name name = FunCanonHeap.get (canon_key name)

  let typedef_pos name = match TypeIdHeap.get name with
    | Some (p, `Typedef) -> Some p
    | Some (_, `Class)
    | None -> None

  let gconst_pos name = ConstPosHeap.get name

end

(* The primitives to manipulate the naming environment *)
module Env = struct
  let check_not_typehint (p, name) =
    let x = canon_key (Utils.strip_all_ns name) in
    match x with
    | x when (
        x = SN.Typehints.void ||
        x = SN.Typehints.noreturn ||
        x = SN.Typehints.int ||
        x = SN.Typehints.bool ||
        x = SN.Typehints.float ||
        x = SN.Typehints.num ||
        x = SN.Typehints.string ||
        x = SN.Typehints.resource ||
        x = SN.Typehints.mixed ||
        x = SN.Typehints.array ||
        x = SN.Typehints.arraykey ||
        x = SN.Typehints.integer ||
        x = SN.Typehints.boolean ||
        x = SN.Typehints.double ||
        x = SN.Typehints.real
      ) -> Errors.name_is_reserved name p; false
    | _ -> true

  let new_fun (p, name) =
    let name_key = canon_key name in
    match FunCanonHeap.get name_key with
    | Some canonical ->
      let p' = FunPosHeap.find_unsafe canonical in
      if not (Pos.compare p p' = 0)
      then Errors.error_name_already_bound name canonical p p'
    | None ->
      FunPosHeap.add name p;
      FunCanonHeap.add name_key name;
      ()

  let new_cid cid_kind (p, name) =
    if not (check_not_typehint (p, name)) then () else
    let name_key = canon_key name in
    match TypeCanonHeap.get name_key with
    | Some canonical ->
      let p' = unsafe_opt @@ GEnv.type_pos canonical in
      if not (Pos.compare p p' = 0)
      then Errors.error_name_already_bound name canonical p p'
    | None ->
      TypeIdHeap.add name (p, cid_kind);
      TypeCanonHeap.add name_key name;
      ()

  let new_class = new_cid `Class

  let new_typedef = new_cid `Typedef

  let new_global_const (p, x) =
    match ConstPosHeap.get x with
    | Some p' ->
      if not (Pos.compare p p' = 0)
      then Errors.error_name_already_bound x x p p';
    | None ->
      ConstPosHeap.add x p;
      ()
end

(*****************************************************************************)
(* Updating the environment *)
(*****************************************************************************)
let remove_decls ~funs ~classes ~typedefs ~consts =
  let canonicalize_set = (fun elt acc -> SSet.add (canon_key elt) acc) in
  let types = SSet.union classes typedefs in
  let canon_types = SSet.fold canonicalize_set types SSet.empty in
  TypeCanonHeap.remove_batch canon_types;
  TypeIdHeap.remove_batch types;

  let fun_namekeys = SSet.fold canonicalize_set funs SSet.empty in
  FunCanonHeap.remove_batch fun_namekeys;
  FunPosHeap.remove_batch funs;

  ConstPosHeap.remove_batch consts

(*****************************************************************************)
(* The entry point to build the naming environment *)
(*****************************************************************************)

let make_env ~funs ~classes ~typedefs ~consts =
  List.iter funs Env.new_fun;
  List.iter classes Env.new_class;
  List.iter typedefs Env.new_typedef;
  List.iter consts Env.new_global_const

(*****************************************************************************)
(* Declaring the names in a list of files *)
(*****************************************************************************)

let add_files_to_rename failed defl defs_in_env =
  List.fold_left ~f:begin fun failed (_, def) ->
    match defs_in_env def with
    | None -> failed
    | Some previous_definition_position ->
      let filename = Pos.filename previous_definition_position in
      Relative_path.Set.add failed filename
  end ~init:failed defl

let ndecl_file fn { FileInfo.file_mode = _; funs; classes; typedefs; consts;
                    consider_names_just_for_autoload; comments = _} =
  let errors, _ = Errors.do_ begin fun () ->
    dn ("Naming decl: "^Relative_path.to_absolute fn);
    if not consider_names_just_for_autoload then
      make_env ~funs ~classes ~typedefs ~consts
  end in
  match errors with
  | [] -> [], Relative_path.Set.empty
  | l ->
  (* IMPORTANT:
   * If a file has name collisions, we MUST add the list of files that
   * were previously defining the type to the set of "failed" files.
   * If we fail to do so, we will be in a phony state, where a name could
   * be missing.
   *
   * Example:
   * A.php defines class A
   * B.php defines class B
   * Save the state, now let's introduce a new file (foo.php):
   * foo.php defines class A and class B.
   *
   * 2 things happen (cf serverTypeCheck.ml):
   * We remove the names A and B from the global environment.
   * We report the error.
   *
   * But this is clearly not enough. If the user removes the file foo.php,
   * both class A and class B are now missing from the naming environment.
   * If the user has a file using class A (in strict), he now gets the
   * error "Unbound name class A".
   *
   * The solution consist in adding all the files that were previously
   * defining the same things as foo.php to the set of files to recheck.
   *
   * This way, when the user removes foo.php, A.php and B.php are recomputed
   * and the naming environment is in a sane state.
   *
   * XXX (jezng): we can probably be less conservative about this -- instead
   * of adding all the declarations in the file, why not just add those that
   * were actually duplicates?
   *)
  let failed = Relative_path.Set.singleton fn in
  let failed = add_files_to_rename failed funs FunPosHeap.get in
  let failed = add_files_to_rename failed classes GEnv.type_pos in
  let failed = add_files_to_rename failed typedefs GEnv.type_pos in
  let failed = add_files_to_rename failed consts ConstPosHeap.get in
  l, failed
