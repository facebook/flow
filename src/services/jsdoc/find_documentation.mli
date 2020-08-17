(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val jsdoc_of_getdef_loc : reader:Parsing_heaps.Reader.reader -> Loc.t -> Jsdoc.t option
