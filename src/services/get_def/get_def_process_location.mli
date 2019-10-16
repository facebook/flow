(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val process_location :
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.program -> Loc.t -> Get_def_request.t option
