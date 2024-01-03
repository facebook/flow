(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type

let mk_seal ~frozen =
  if frozen then
    Object.Spread.Frozen
  else
    Object.Spread.Sealed

let mk_with_proto
    cx
    reason
    ~obj_kind
    ?(frozen = false)
    ?reachable_targs
    ?call
    ?(props = NameUtils.Map.empty)
    ?id
    proto =
  let flags = { obj_kind; frozen; react_dro = None } in
  let call = Base.Option.map call ~f:(Context.make_call_prop cx) in
  let pmap =
    match id with
    | None -> Context.generate_property_map cx props
    | Some id ->
      Context.add_property_map cx id props;
      id
  in
  DefT (reason, ObjT (mk_objecttype ?reachable_targs ~flags ~call pmap proto))

let mk_exact_empty cx reason =
  ObjProtoT reason |> mk_with_proto cx reason ~obj_kind:Exact ~frozen:true

let mk ~obj_kind cx reason = mk_with_proto cx ~obj_kind reason (ObjProtoT reason)

let is_exact = function
  | Exact -> true
  | _ -> false

let get_dict_opt = function
  | Indexed d -> Some d
  | Exact
  | Inexact ->
    None

let map_dict f kind =
  match kind with
  | Indexed d -> Indexed (f d)
  | Exact
  | Inexact ->
    kind

let obj_kind_from_optional_dict ~dict ~otherwise =
  match dict with
  | Some d -> Indexed d
  | None -> otherwise
