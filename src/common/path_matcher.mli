(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val empty : t
val paths : t -> Path.t list
val stems : t -> Path.t list
val stem_map : t -> ((string * Str.regexp) list) Utils_js.PathMap.t

val add : t -> Path.t -> t
val matches : t -> string -> bool
