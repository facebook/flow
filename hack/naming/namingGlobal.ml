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

  let class_id name = ClassIdHeap.get name

  let class_canon_name name = ClassCanonHeap.get (canon_key name)

  let fun_id name = FunIdHeap.get name

  let fun_canon_name name = FunCanonHeap.get (canon_key name)

  let typedef_id name = TypedefIdHeap.get name

  let gconst_id name = ConstIdHeap.get name

end

(* The primitives to manipulate the naming environment *)
module Env = struct
  let classes =
    (module ClassIdHeap : IdHeap), (module ClassCanonHeap : CanonHeap)

  let funs = (module FunIdHeap : IdHeap), (module FunCanonHeap : CanonHeap)

  let gconsts = (module ConstIdHeap : IdHeap)

  let resilient_new_canon_var (id_heap, canon_heap) (p, name) =
    let module Ids = (val id_heap : IdHeap) in
    let module Canons = (val canon_heap : CanonHeap) in
    let name_key = canon_key name in
    match Canons.get name_key with
      | Some canonical ->
        let p', id = Ids.find_unsafe canonical in
        if Pos.compare p p' = 0 then (p, id)
        else begin
          Errors.error_name_already_bound name canonical p p';
          p', id
        end
      | None ->
        let pos_and_id = p, Ident.make name in
        Ids.add name pos_and_id;
        Canons.add name_key name;
        pos_and_id

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

  let resilient_new_var id_heap (p, x) =
    let module Ids = (val id_heap : IdHeap) in
    match Ids.get x with
    | Some (p', y) ->
      if Pos.compare p p' = 0 then (p, y)
      else begin
        Errors.error_name_already_bound x x p p';
        p', y
      end
   | None ->
      let y = p, Ident.make x in
      Ids.add x y;
      y

  let new_fun_id x =
    ignore (resilient_new_canon_var funs x)

  let new_class_id x =
    if check_not_typehint x then ignore (resilient_new_canon_var classes x)
    else ()

  let new_typedef_id x =
    if check_not_typehint x
    then
      let v = resilient_new_canon_var classes x in
      TypedefIdHeap.add (snd x) v
    else ()

  let new_global_const_id x =
    let v = resilient_new_var gconsts x in
    ConstIdHeap.add (snd x) v
end

(*****************************************************************************)
(* Updating the environment *)
(*****************************************************************************)
let remove_decls ~funs ~classes ~typedefs ~consts =
  let canonicalize_set = (fun elt acc -> SSet.add (canon_key elt) acc) in

  let class_namekeys = SSet.fold canonicalize_set classes SSet.empty in
  let class_namekeys = SSet.fold canonicalize_set typedefs class_namekeys in
  ClassCanonHeap.remove_batch class_namekeys;
  ClassIdHeap.remove_batch classes;

  let fun_namekeys = SSet.fold canonicalize_set funs SSet.empty in
  FunCanonHeap.remove_batch fun_namekeys;
  FunIdHeap.remove_batch funs;

  TypedefIdHeap.remove_batch typedefs;
  ConstIdHeap.remove_batch consts

(*****************************************************************************)
(* The entry point to build the naming environment *)
(*****************************************************************************)

let make_env ~funs ~classes ~typedefs ~consts =
  List.iter funs Env.new_fun_id;
  List.iter classes Env.new_class_id;
  List.iter typedefs Env.new_typedef_id ;
  List.iter consts Env.new_global_const_id

(*****************************************************************************)
(* Declaring the names in a list of files *)
(*****************************************************************************)

let add_files_to_rename failed defl defs_in_env =
  List.fold_left ~f:begin fun failed (_, def) ->
    match defs_in_env def with
    | None -> failed
    | Some (previous_definition_position, _) ->
      let filename = Pos.filename previous_definition_position in
      Relative_path.Set.add filename failed
  end ~init:failed defl

let ndecl_file fn { FileInfo.file_mode; funs; classes; typedefs; consts;
                    consider_names_just_for_autoload; comments } =
  let errors, _ = Errors.do_ begin fun () ->
    dn ("Naming decl: "^Relative_path.to_absolute fn);
    if not consider_names_just_for_autoload then
      make_env ~funs ~classes ~typedefs ~consts
  end
  in
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
   *)
  let failed = Relative_path.Set.singleton fn in
  let failed = add_files_to_rename failed funs FunIdHeap.get in
  let failed = add_files_to_rename failed classes ClassIdHeap.get in
  let failed = add_files_to_rename failed typedefs TypedefIdHeap.get in
  let failed = add_files_to_rename failed consts ConstIdHeap.get in
  l, failed
