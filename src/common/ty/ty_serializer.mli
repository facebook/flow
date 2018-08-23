(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val type_ : Ty.t -> ((Loc.t, Loc.t) Flow_ast.Type.t, string) Core_result.t
