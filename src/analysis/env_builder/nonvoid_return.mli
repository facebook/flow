(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val might_have_nonvoid_return : ALoc.t -> (ALoc.t, ALoc.t) Flow_ast.Function.t -> bool
