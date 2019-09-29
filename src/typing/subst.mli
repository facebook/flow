(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val subst : Context.t -> ?use_op:Type.use_op -> ?force:bool -> Type.t SMap.t -> Type.t -> Type.t
