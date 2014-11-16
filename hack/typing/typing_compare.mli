(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils
open Typing_defs
open Typing_env

val get_extend_deps : ISet.elt -> ISet.t -> ISet.t

val get_classes_deps : class_type option SMap.t -> class_type option SMap.t ->
  SSet.t -> ISet.t * ISet.t

val get_funs_deps : fun_type option SMap.t -> SSet.t -> ISet.t * ISet.t

val get_types_deps : Typedef.tdef_or_error option SMap.t -> SSet.t -> ISet.t

val get_gconsts_deps : GConsts.t option SMap.t -> SSet.t -> ISet.t * ISet.t
