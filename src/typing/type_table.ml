(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type id_kind =
  | PropertyAccess of Type.t (* receiver type *)
  | Other

type id_info = string (* name *) * Type.t * id_kind

type t = {
  (* This stores type information about expressions. Every expression in the program should have a
   * type here. This means that there is some nesting, e.g. in the expression `5 + 4` there will be
   * a type for `5`, a type for `4`, and a type for the entire addition expression. *)
  coverage: (Loc.t, Type.t) Hashtbl.t;
  (* This stores type information about identifiers only. There should be no overlap or nesting of
   * locations here. *)
  type_info: (Loc.t, id_info) Hashtbl.t;
}

let create () = {
  coverage = Hashtbl.create 0;
  type_info = Hashtbl.create 0;
}

let set {coverage; _} loc value =
  Hashtbl.replace coverage loc value

let set_info {type_info; _} loc value =
  Hashtbl.replace type_info loc value

let fold_coverage f t init = Hashtbl.fold f t.coverage init

let find_unsafe_coverage t k = Hashtbl.find t.coverage k

let reset {coverage; type_info} =
  Hashtbl.reset coverage;
  Hashtbl.reset type_info

let copy {coverage; type_info} = {
  coverage = Hashtbl.copy coverage;
  type_info = Hashtbl.copy type_info;
}

let find_type_info ~pred t =
  Hashtbl.fold (fun k v a ->
    if pred k then Some (k, v) else a
  ) t.type_info None

let coverage_to_list t =
  let r = ref [] in
  Hashtbl.iter (fun l t -> r := (l, t) :: !r) t.coverage;
  !r
