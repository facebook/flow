(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val json_of_sourcemap : Sourcemap.t -> Hh_json.json

val sourcemap_of_json : Hh_json.json -> Sourcemap.t

val sourcemap_of_string : string -> Sourcemap.t
