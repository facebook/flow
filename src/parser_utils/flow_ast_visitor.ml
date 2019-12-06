(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let run : 'node. ('node -> 'node) -> 'node -> unit = (fun visit node -> ignore @@ visit node)

let run_opt : 'node. ('node -> 'node) -> 'node option -> unit =
 (fun visit -> Base.Option.iter ~f:(run visit))

let run_list : 'node. ('node -> 'node) -> 'node list -> unit = (fun visit -> List.iter (run visit))

class ['acc, 'loc] visitor ~init =
  object (this)
    inherit ['loc] Flow_ast_mapper.mapper

    val mutable acc : 'acc = init

    method acc = acc

    method set_acc x = acc <- x

    method update_acc f = acc <- f acc

    method eval : 'node. ('node -> 'node) -> 'node -> 'acc =
      fun visit node ->
        run visit node;
        this#acc
  end
