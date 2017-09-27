(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t
val empty: t
val add: 'a -> t -> t
val add_type: Type.t -> t -> t
val add_use: Type.use_t -> t -> t
val add_props_map: Type.Properties.t -> t -> t
val add_exports_map: Type.Exports.t -> t -> t
