(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Utils

(* returns a grounded(, normalized) and printified version of the type *)
val printified_type: Context.t -> Type.t -> Type.t

val suggested_type_cache: Type.t IMap.t ref
