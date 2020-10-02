(*
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
  | Library of remote_info
  | Builtin

and symbol = {
  sym_provenance: provenance;
  sym_def_loc: ALoc.t; [@printer (fun fmt loc -> fprintf fmt "%s" (ALoc.to_string_no_source loc))]
  sym_name: string;
  sym_anonymous: bool;
}
[@@deriving show]

let builtin_symbol name =
  { sym_provenance = Builtin; sym_def_loc = ALoc.none; sym_name = name; sym_anonymous = false }
