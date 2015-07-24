(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Various constants and the like; normally copied from HHVM *)

open Utils

(* the enum values from hphp/runtime/base/header-kind.h;
 * XXX: this is expedient but we will need to do something better to
 * keep this synced eventually *)
let header_kinds = [
  "Packed"; "Struct"; "Mixed"; "Empty"; "Apc"; "Globals"; "Proxy"; "String";
  "Resource"; "Ref"; "Object"; "ResumableObj"; "AwaitAllWH";
  "Vector"; "Map"; "Set"; "Pair"; "ImmVector"; "ImmMap"; "ImmSet";
  "ResumableFrame"; "NativeData";
  "SmallMalloc"; "BigMalloc"; "BigObj"; "Free"; "Hole"
]
let header_kind_values = Core_list.foldi
  ~f:(fun i m kind -> SMap.add kind i m) ~init:SMap.empty header_kinds

(* not all of the header kinds are collections, but type typechecker
 * won't pass bogus ones *)
let get_collection_id s = match SMap.get s header_kind_values with
  | Some i -> i
  | None -> Emitter_core.bug "invalid collection name"
