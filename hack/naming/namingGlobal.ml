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

module SN = Naming_special_names

(*****************************************************************************)
(* The types *)
(*****************************************************************************)

type fun_set = Utils.SSet.t
type class_set = Utils.SSet.t
type typedef_set = Utils.SSet.t
type const_set = Utils.SSet.t
type decl_set = fun_set * class_set * typedef_set * const_set

(* We want to keep the positions of names that have been
 * replaced by identifiers.
 *)
type positioned_ident = (Pos.t * Ident.t)
type map = positioned_ident SMap.t
type canon_names_map = string SMap.t
let canon_key = String.lowercase

type genv = {
  (* Set of class names defined, and their positions *)
  classes: (map * canon_names_map) ref;

  (* Set of function names defined, and their positions *)
  funs: (map * canon_names_map) ref;

  (* Set of typedef names defined, and their position *)
  typedefs: map ref;

  (* Set of constant names defined, and their position *)
  gconsts: map ref;
}

type env = {
  iclasses: map * canon_names_map;
  ifuns: map * canon_names_map;
  itypedefs: map;
  iconsts: map;
}

module GEnv = struct

  type t = env

  let class_id env name =
    let classes, _ = env.iclasses in
    SMap.get name classes

  let class_canon_name env name =
    let _, classes = env.iclasses in
    SMap.get (canon_key name) classes

  let fun_id env name =
    let funs, _ = env.ifuns in
    SMap.get name funs

  let fun_canon_name env name =
    let _, funs = env.ifuns in
    SMap.get (canon_key name) funs

  let typedef_id env name =
    SMap.get name env.itypedefs
  let gconst_id env name =
    SMap.get name env.iconsts
end

(*****************************************************************************)
(* Empty (initial) environments *)
(*****************************************************************************)

let empty = {
  iclasses  = SMap.empty, SMap.empty;
  ifuns     = SMap.empty, SMap.empty;
  itypedefs = SMap.empty;
  iconsts   = SMap.empty;
}

(* The primitives to manipulate the naming environment *)
module Env = struct

  let empty_global nenv = {
    classes       = ref nenv.iclasses;
    funs          = ref nenv.ifuns;
    typedefs      = ref nenv.itypedefs;
    gconsts       = ref nenv.iconsts;
  }

  let resilient_new_canon_var env_and_names (p, name) =
    let env, canon_names = !env_and_names in
    let name_key = canon_key name in
    match SMap.get name_key canon_names with
      | Some canonical ->
        let p', id = SMap.find_unsafe canonical env in
        if Pos.compare p p' = 0 then (p, id)
        else begin
          Errors.error_name_already_bound name canonical p p';
          p', id
        end
      | None ->
        let pos_and_id = p, Ident.make name in
        env_and_names :=
          SMap.add name pos_and_id env, SMap.add name_key name canon_names;
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

  let resilient_new_var env (p, x) =
    if SMap.mem x !env
    then begin
      let p', y = SMap.find_unsafe x !env in
      if Pos.compare p p' = 0 then (p, y)
      else begin
        Errors.error_name_already_bound x x p p';
        p', y
      end
    end
    else
      let y = p, Ident.make x in
      env := SMap.add x y !env;
      y

  let new_fun_id genv x =
    ignore (resilient_new_canon_var genv.funs x)

  let new_class_id genv x =
    if check_not_typehint x then ignore (resilient_new_canon_var genv.classes x)
    else ()

  let new_typedef_id genv x =
    if check_not_typehint x
    then begin
      let v = resilient_new_canon_var genv.classes x in
      genv.typedefs := SMap.add (snd x) v !(genv.typedefs);
      ()
    end
    else ()

  let new_global_const_id genv x =
    let v = resilient_new_var genv.gconsts x in
    genv.gconsts := SMap.add (snd x) v !(genv.gconsts);
    ()
end

(*****************************************************************************)
(* Updating the environment *)
(*****************************************************************************)
let remove_decls env (funs, classes, typedefs, consts) =
  let canonicalize_set = (fun elt acc -> SSet.add (canon_key elt) acc) in
  let class_namekeys = SSet.fold canonicalize_set classes SSet.empty in
  let typedef_namekeys = SSet.fold canonicalize_set typedefs SSet.empty in
  let fun_namekeys = SSet.fold canonicalize_set funs SSet.empty in
  let iclassmap, iclassnames = env.iclasses in
  let iclassmap, iclassnames =
    SSet.fold SMap.remove classes iclassmap,
    SSet.fold SMap.remove class_namekeys iclassnames
  in
  let iclassmap, iclassnames =
    SSet.fold SMap.remove typedefs iclassmap,
    SSet.fold SMap.remove typedef_namekeys iclassnames
  in
  let ifunmap, ifunnames = env.ifuns in
  let ifunmap, ifunnames =
    SSet.fold SMap.remove funs ifunmap,
    SSet.fold SMap.remove fun_namekeys ifunnames
  in
  let itypedefs = SSet.fold SMap.remove typedefs env.itypedefs in
  let iconsts = SSet.fold SMap.remove consts env.iconsts in
  {
    ifuns     = ifunmap, ifunnames;
    iclasses  = iclassmap, iclassnames;
    itypedefs = itypedefs;
    iconsts   = iconsts;
  }

(*****************************************************************************)
(* The entry point to build the naming environment *)
(*****************************************************************************)

let make_env old_env ~funs ~classes ~typedefs ~consts =
  let genv = Env.empty_global old_env in
  List.iter funs (Env.new_fun_id genv);
  List.iter classes (Env.new_class_id genv);
  List.iter typedefs (Env.new_typedef_id genv);
  List.iter consts (Env.new_global_const_id genv);
  let new_env = {
    iclasses = !(genv.classes);
    ifuns = !(genv.funs);
    itypedefs = !(genv.typedefs);
    iconsts = !(genv.gconsts);
  } in
  new_env

(*****************************************************************************)
(* Declaring the names in a list of files *)
(*****************************************************************************)

let add_files_to_rename nenv failed defl defs_in_env =
  List.fold_left ~f:begin fun failed (_, def) ->
    match SMap.get def defs_in_env with
    | None -> failed
    | Some (previous_definition_position, _) ->
      let filename = Pos.filename previous_definition_position in
      Relative_path.Set.add filename failed
  end ~init:failed defl

let ndecl_file fn
    {FileInfo.file_mode; funs;
     classes; typedefs; consts; consider_names_just_for_autoload; comments}
    nenv =
  let errors, nenv = Errors.do_ begin fun () ->
    dn ("Naming decl: "^Relative_path.to_absolute fn);
    if consider_names_just_for_autoload
    then nenv
    else make_env nenv ~funs ~classes ~typedefs ~consts
  end
  in
  match errors with
  | [] -> [], Relative_path.Set.empty, nenv
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
  let failed = add_files_to_rename nenv failed funs (fst nenv.ifuns) in
  let failed = add_files_to_rename nenv failed classes (fst nenv.iclasses) in
  let failed = add_files_to_rename nenv failed typedefs nenv.itypedefs in
  let failed = add_files_to_rename nenv failed consts nenv.iconsts in
  l, failed, nenv
