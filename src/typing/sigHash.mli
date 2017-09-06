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
val empty: t
val add: 'a -> t -> t
val add_type: Type.t -> t -> t
val add_use: Type.use_t -> t -> t
val add_props_map: Type.Properties.t -> t -> t
val add_exports_map: Type.Exports.t -> t -> t
