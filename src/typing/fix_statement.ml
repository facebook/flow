(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module rec Statement_ : Statement_sig.S =
  Statement.Make (Destructuring_) (Func_stmt_config_) (Statement_)

and Destructuring_ : Destructuring_sig.S = Destructuring.Make (Statement_)

and Func_stmt_config_ : (Func_stmt_config_sig.S with module Types := Func_stmt_config_types.Types) =
  Func_stmt_config.Make (Destructuring_) (Statement_)

(* Some versions of Ocaml raise a warning 60 (unused module) without the following *)
module _ = Destructuring_
module _ = Func_stmt_config_
