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

and imported_ident =
  (ALoc.t * string * import_mode
  [@printer (fun fmt (_, id, _) -> fprintf fmt "%s" id)])

and remote_info = { imported_as: imported_ident option }

and provenance =
  | Local
  | Remote of remote_info
  | Library
  | Builtin

and symbol = {
  provenance: provenance;
  def_loc: ALoc.t; [@printer (fun fmt loc -> fprintf fmt "%s" (ALoc.to_string_no_source loc))]
  name: string;
  anonymous: bool;
}
[@@deriving show]

let builtin_symbol name = { provenance = Builtin; def_loc = ALoc.none; name; anonymous = false }
