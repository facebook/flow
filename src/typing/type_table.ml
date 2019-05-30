(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type id_kind =
  | PropertyAccess of Type.t (* receiver type *)
  | Import of string (* remote name *) * Type.t (* module type *)
  | Exists
  | Other

type name = string

type 'a entry = name * 'a * id_kind


type type_entry = Type.t entry
type scheme_entry = Type.TypeScheme.t entry

type t = {
  (* This stores type information about identifiers only. There should be no overlap or nesting of
   * locations here. *)
  type_info: (ALoc.t, Type.TypeScheme.t entry) Hashtbl.t;
  (* Keep a stack of the type parameters in scope and use it to create type schemes. *)
  tparams: Type.typeparam list ref;
}

let create () = {
  type_info = Hashtbl.create 0;
  tparams = ref [];
}

(* Insert a located tuple into the type_info hashtable (intended for type-at-pos).
 * In certain contexts it is useful to allow the caller to provide some additional
 * type parameters that should be in scope when reconstructing this type. See for
 * example the case of generic functions. `extra_tparams` can be used to pass this
 * additional environment. *)
let set_info ?extra_tparams loc (name, t, i) x =
  let {type_info; tparams; _} = x in
  let extra_tparams = Option.value ~default:[] extra_tparams in
  let tparams = extra_tparams @ !tparams in
  let scheme = { Type.TypeScheme.tparams; type_ = t } in
  Hashtbl.replace type_info loc (name, scheme, i)

let reset {type_info; tparams} =
  Hashtbl.reset type_info;
  tparams := []

let copy {type_info; tparams} = {
  type_info = Hashtbl.copy type_info;
  tparams = ref !tparams;
}

let with_typeparams new_tparams x f =
  let old_tparams = !(x.tparams) in
  x.tparams := new_tparams @ old_tparams;
  let r = f () in
  x.tparams := old_tparams;
  r

let find_type_info t loc =
  match Hashtbl.find t.type_info loc with
  | exception Not_found -> None
  | x -> Some x

let find_type_info_with_pred t pred =
  Hashtbl.fold (fun k v a ->
    if pred k then Some (k, v) else a
  ) t.type_info None

let type_info_hashtbl t =
  t.type_info
