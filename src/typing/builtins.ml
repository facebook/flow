(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  original_names: Type.t lazy_t SMap.t;
  original_modules: Type.t lazy_t SMap.t;
  mapper: Type.t -> Type.t;
  mapped_names: (string, Type.t) Hashtbl.t;
  mapped_modules: (string, Type.t) Hashtbl.t;
}

let builtin_ordinary_name_set { original_names; _ } =
  SMap.fold (fun k _ acc -> SSet.add k acc) original_names SSet.empty

let get_builtin_name_opt
    { original_names; original_modules = _; mapper; mapped_names; mapped_modules = _ } name =
  match Hashtbl.find_opt mapped_names name with
  | Some v -> Some v
  | None ->
    (match SMap.find_opt name original_names with
    | None -> None
    | Some (lazy v) ->
      let v = mapper v in
      Hashtbl.add mapped_names name v;
      Some v)

let get_builtin_module_opt
    { original_names = _; original_modules; mapper; mapped_names = _; mapped_modules } name =
  match Hashtbl.find_opt mapped_modules name with
  | Some v -> Some v
  | None ->
    (match SMap.find_opt name original_modules with
    | None -> None
    | Some (lazy v) ->
      let v = mapper v in
      Hashtbl.add mapped_modules name v;
      Some v)

let of_name_map ~mapper original_names original_modules =
  {
    original_names;
    original_modules;
    mapper;
    mapped_names = Hashtbl.create 0;
    mapped_modules = Hashtbl.create 0;
  }

let empty () : t = of_name_map ~mapper:Base.Fn.id SMap.empty SMap.empty
