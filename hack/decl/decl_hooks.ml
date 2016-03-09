(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

let (class_id_hooks: (Pos.t * string ->
                      (Pos.t * string) option -> unit) list ref) = ref []

let attach_class_id_hook hook =
  class_id_hooks := hook :: !class_id_hooks

let dispatch_class_id_hook c_id m_id_optional =
  List.iter !class_id_hooks begin fun hook -> hook c_id m_id_optional end

let remove_all_hooks () =
  class_id_hooks := [];
