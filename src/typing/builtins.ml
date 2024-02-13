(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  original: Type.t lazy_t NameUtils.Map.t;
  mapper: Type.t -> Type.t;
  mapped: (Reason.name, Type.t) Hashtbl.t;
}

let builtin_set { original; _ } =
  NameUtils.Map.fold (fun k _ acc -> NameUtils.Set.add k acc) original NameUtils.Set.empty

let get_builtin_opt { original; mapper; mapped } name =
  match Hashtbl.find_opt mapped name with
  | Some v -> Some v
  | None ->
    (match NameUtils.Map.find_opt name original with
    | None -> None
    | Some (lazy v) ->
      let v = mapper v in
      Hashtbl.add mapped name v;
      Some v)

let get_builtin_name_opt builtins name = get_builtin_opt builtins (Reason.OrdinaryName name)

let get_builtin_module_opt builtins name = get_builtin_opt builtins (Reason.InternalModuleName name)

let of_name_map ~mapper original = { original; mapper; mapped = Hashtbl.create 0 }

let empty () : t = of_name_map ~mapper:Base.Fn.id NameUtils.Map.empty
