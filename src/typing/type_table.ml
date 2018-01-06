(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)


type t = {
  coverage: (Loc.t, Type.t) Hashtbl.t;
  type_info: (Loc.t, Type.t) Hashtbl.t;
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

let find_all_type_info f t =
  let r = ref [] in
  Hashtbl.iter (fun k v ->
    if f k v then r := (k, v) :: !r
  ) t.type_info;
  !r
