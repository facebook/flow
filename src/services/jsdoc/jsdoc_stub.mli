(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val stub_for_function : ('M, 'T) Flow_ast.Function.t -> t

val string_of_stub : t -> string
