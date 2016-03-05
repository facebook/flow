(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

open IdeJson

type deferred_to_typechecker =
  | FindRefsCall of FindRefsService.action

type result =
  | Result of IdeJson.response_type
  | DeferredToTypechecker of deferred_to_typechecker

val get_call_response:
  call_id ->
  call_type ->
  FileInfo.t Relative_path.Map.t ->
  Errors.t ->
  result
