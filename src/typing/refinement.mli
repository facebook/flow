(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val key : ('loc, 'loc) Flow_ast.Expression.t -> Key.t option

val get : Context.t -> ('loc, 'loc) Flow_ast.Expression.t -> ALoc.t -> Type.t option

val key_of_pattern : ('loc, 'loc) Flow_ast.Pattern.t -> Key.t option

val get_of_pattern : Context.t -> ('loc, 'loc) Flow_ast.Pattern.t -> ALoc.t -> Type.t option
