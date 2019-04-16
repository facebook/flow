(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type import_mode =
 | ValueMode
 | TypeMode
 | TypeofMode

type imported_ident = ALoc.t * string * import_mode

type remote_info = {
  imported_as: imported_ident option;
}

type provenance =
  | Local
  | Remote of remote_info
  | Library
  | Builtin

type symbol = {
  provenance: provenance;
  def_loc: ALoc.t;
  name: string;
  anonymous: bool;
}

let builtin_symbol name = {
  provenance = Builtin;
  def_loc = ALoc.none;
  name;
  anonymous = false;
}
