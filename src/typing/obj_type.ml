(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type

let mk_with_proto cx reason
  ?(sealed=false) ?(exact=true) ?(frozen=false) ?dict ?(props=SMap.empty) proto =
  let sealed =
    if sealed then Sealed
    else UnsealedInFile (Loc.source (Reason.loc_of_reason reason))
  in
  let flags = { sealed; exact; frozen } in
  let pmap = Context.make_property_map cx props in
  DefT (reason, ObjT (mk_objecttype ~flags dict pmap proto))

let mk cx reason =
  mk_with_proto cx reason (ObjProtoT reason)
