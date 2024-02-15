(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  original_global_values: Type.t lazy_t SMap.t;
  original_global_types: Type.t lazy_t SMap.t;
  original_global_modules: Type.t lazy_t SMap.t;
  mapper: Type.t -> Type.t;
  mapped_global_names: (string, Type.t) Hashtbl.t;
  mapped_global_types: (string, Type.t) Hashtbl.t;
  mapped_global_modules: (string, Type.t) Hashtbl.t;
}

let builtin_ordinary_name_set { original_global_values; original_global_types; _ } =
  let add = SMap.fold (fun k _ acc -> SSet.add k acc) in
  SSet.empty |> add original_global_values |> add original_global_types

let get_builtin_value_opt { original_global_values; mapper; mapped_global_names; _ } name =
  match Hashtbl.find_opt mapped_global_names name with
  | Some v -> Some v
  | None ->
    (match SMap.find_opt name original_global_values with
    | None -> None
    | Some (lazy v) ->
      let v = mapper v in
      Hashtbl.add mapped_global_names name v;
      Some v)

let get_builtin_type_opt { original_global_types; mapper; mapped_global_types; _ } name =
  match Hashtbl.find_opt mapped_global_types name with
  | Some v -> Some v
  | None ->
    (match SMap.find_opt name original_global_types with
    | None -> None
    | Some (lazy v) ->
      let v = mapper v in
      Hashtbl.add mapped_global_types name v;
      Some v)

let get_builtin_type_opt builtins name =
  match get_builtin_type_opt builtins name with
  | None -> get_builtin_value_opt builtins name
  | v_opt -> v_opt

let get_builtin_module_opt { original_global_modules; mapper; mapped_global_modules; _ } name =
  match Hashtbl.find_opt mapped_global_modules name with
  | Some v -> Some v
  | None ->
    (match SMap.find_opt name original_global_modules with
    | None -> None
    | Some (lazy v) ->
      let v = mapper v in
      Hashtbl.add mapped_global_modules name v;
      Some v)

let of_name_map ~mapper ~values ~types ~modules =
  {
    original_global_values = values;
    original_global_types = types;
    original_global_modules = modules;
    mapper;
    mapped_global_names = Hashtbl.create 0;
    mapped_global_types = Hashtbl.create 0;
    mapped_global_modules = Hashtbl.create 0;
  }

let empty () : t =
  of_name_map ~mapper:Base.Fn.id ~values:SMap.empty ~types:SMap.empty ~modules:SMap.empty
