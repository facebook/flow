(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type

let mk_with_proto cx reason
  ?(sealed=false) ?(exact=true) ?(frozen=false) ?dict ?call ?(props=SMap.empty) proto =
  let sealed =
    if sealed then Sealed
    else UnsealedInFile (ALoc.source (Reason.aloc_of_reason reason))
  in
  let flags = { sealed; exact; frozen } in
  let call = Option.map call ~f:(Context.make_call_prop cx) in
  let pmap = Context.make_property_map cx props in
  DefT (reason, bogus_trust (), ObjT (mk_objecttype ~flags ~dict ~call pmap proto))

let mk_exact_empty cx reason =
  ObjProtoT reason |> mk_with_proto cx reason ~sealed:true ~exact:true ~frozen:true

let mk ?(sealed=false) cx reason =
  mk_with_proto cx reason ~sealed (ObjProtoT reason)

and sealed_in_op reason_op = function
  | Sealed -> true
  | UnsealedInFile source -> source <> (ALoc.source (Reason.aloc_of_reason reason_op))
