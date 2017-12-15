(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* returns a grounded(, normalized) and printified version of the type *)
val normalize_type: Context.t -> Type.t -> Type.t

val suggested_type_cache: Type.t IMap.t ref
