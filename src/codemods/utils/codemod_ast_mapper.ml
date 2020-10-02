(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type name = string

class ['acc] mapper (n : name) ~(init : 'acc) =
  object (_this)
    inherit ['acc, Loc.t] Flow_ast_visitor.visitor ~init

    val log = (fun s -> Hh_logger.info "{%s}: %s%!" n s)
  end
