(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type

let mk_seal reason = function
  | true -> Object.Spread.Sealed
  | false -> Object.Spread.UnsealedInFile (ALoc.source (Reason.aloc_of_reason reason))

let mk_with_proto cx reason ~obj_kind ?(frozen = false) ?call ?(props = SMap.empty) ?loc proto =
  let flags = { obj_kind; frozen } in
  let call = Base.Option.map call ~f:(Context.make_call_prop cx) in
  let pmap =
    match loc with
    | None -> Context.generate_property_map cx props
    | Some loc -> Context.make_source_property_map cx props loc
  in
  DefT (reason, bogus_trust (), ObjT (mk_objecttype ~flags ~call pmap proto))

let mk_unsealed cx ?loc ?proto ?(props = SMap.empty) reason =
  let proto =
    match proto with
    | None -> ObjProtoT reason
    | Some proto -> proto
  in
  let obj_kind = UnsealedInFile (ALoc.source (Reason.aloc_of_reason reason)) in
  mk_with_proto cx reason ~obj_kind ~props ?loc proto

let mk_exact_empty cx reason =
  ObjProtoT reason |> mk_with_proto cx reason ~obj_kind:Exact ~frozen:true

let mk ~obj_kind cx reason = mk_with_proto cx ~obj_kind reason (ObjProtoT reason)

and sealed_in_op reason_op = function
  | UnsealedInFile source -> source <> ALoc.source (Reason.aloc_of_reason reason_op)
  | _ -> true

let is_exact = function
  | Exact -> true
  | _ -> false

let is_exact_or_sealed reason obj_kind =
  match obj_kind with
  | Exact -> true
  | UnsealedInFile _ -> sealed_in_op reason obj_kind
  | _ -> false

let get_dict_opt = function
  | Indexed d -> Some d
  | Exact
  | Inexact
  | UnsealedInFile _ ->
    None

let map_dict f kind =
  match kind with
  | Indexed d -> Indexed (f d)
  | Exact
  | Inexact
  | UnsealedInFile _ ->
    kind

let obj_kind_from_optional_dict ~dict ~otherwise =
  match dict with
  | Some d -> Indexed d
  | None -> otherwise

(* Before we had the obj_kind field, we had a sealed field and an exact boolean field.
 * We had an implicit invariant where unsealed => exact and inexact => sealed. In several places,
 * our code only checked that something's exact flag was set to true. In these cases, unsealed
 * objects may also pass those checks unintentionally. In order to preserve that behavior when
 * we restructured our object types, we introduced this function, which minimized the error diff.
 *)
let is_legacy_exact_DO_NOT_USE = function
  | UnsealedInFile _
  | Exact ->
    true
  | _ -> false
