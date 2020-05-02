(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type options = unit

val type_ : options -> Ty.t -> ((Loc.t, Loc.t) Flow_ast.Type.t, string) result
