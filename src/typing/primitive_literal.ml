(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type syntactic_flags = {
  cond: Type.cond_context option;
  decl: Ast.Variable.kind option;
  as_const: bool;
  frozen: Type.frozen_kind;
  has_hint: bool Lazy.t;
}

let empty_syntactic_flags =
  { cond = None; decl = None; as_const = false; frozen = Type.NotFrozen; has_hint = lazy false }

let mk_syntactic_flags
    ?cond ?decl ?(as_const = false) ?(frozen = Type.NotFrozen) ?(has_hint = lazy false) () =
  { cond; decl; as_const; frozen; has_hint }

let loc_has_hint _cx _loc = (* TODO *) false
