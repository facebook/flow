(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val detect_errors : Context.t -> (ALoc.t, ALoc.t) Flow_ast.Program.t -> Context.metadata -> unit
