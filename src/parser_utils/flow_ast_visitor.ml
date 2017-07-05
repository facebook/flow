(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let run: 'node. ('node -> 'node) -> 'node -> unit =
  fun visit node -> ignore @@ (visit node)

let run_opt: 'node. ('node -> 'node) -> 'node option -> unit =
  fun visit -> Option.iter ~f:(run visit)

let run_list: 'node. ('node -> 'node) -> 'node list -> unit =
  fun visit -> List.iter (run visit)

class ['acc] visitor ~init = object(this)
  inherit Flow_ast_mapper.mapper

  val mutable acc: 'acc = init
  method acc = acc
  method set_acc x = acc <- x
  method update_acc f = acc <- f acc

  method eval: 'node. ('node -> 'node) -> 'node -> 'acc =
    fun visit node ->
      run visit node;
      this#acc
end
