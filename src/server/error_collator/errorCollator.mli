(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val regenerate: ServerEnv.env -> ServerEnv.collated_errors

val get_with_separate_warnings:
  ServerEnv.env ->
  Errors.ConcreteLocPrintableErrorSet.t * Errors.ConcreteLocPrintableErrorSet.t Utils_js.FilenameMap.t * (Loc.t Errors.printable_error * Utils_js.LocSet.t) list

val get:
  ServerEnv.env ->
  Errors.ConcreteLocPrintableErrorSet.t * Errors.ConcreteLocPrintableErrorSet.t * (Loc.t Errors.printable_error * Utils_js.LocSet.t) list
