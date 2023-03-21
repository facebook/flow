(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val hoist_infer_types : ('M, 'T) Flow_ast.Type.t -> ('T * ('M, 'T) Flow_ast.Type.Infer.t) list
