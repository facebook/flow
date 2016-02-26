(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t

val empty : t
val paths : t -> Path.t list
val stems : t -> Path.t list
val stem_map : t -> ((string * Str.regexp) list) Utils_js.PathMap.t

val add : t -> Path.t -> t
val matches : t -> string -> bool
