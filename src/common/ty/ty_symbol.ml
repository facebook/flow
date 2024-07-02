(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type import_mode =
  | ValueMode
  | TypeMode
  | TypeofMode

and 'loc imported_ident_ = 'loc * string * import_mode

and 'loc remote_info_ = { imported_as: 'loc imported_ident_ option }

and 'loc provenance_ =
  | Local
  | Remote of 'loc remote_info_
  | Library of 'loc remote_info_
  | Builtin

and 'loc symbol_ = {
  sym_provenance: 'loc provenance_;
  sym_def_loc: 'loc;
  sym_name: Reason.name;
  sym_anonymous: bool;
}

and imported_ident = ALoc.t imported_ident_

and remote_info = ALoc.t remote_info_

and provenance = ALoc.t provenance_

and symbol = ALoc.t symbol_ [@@deriving show]

let builtin_symbol name =
  { sym_provenance = Builtin; sym_def_loc = ALoc.none; sym_name = name; sym_anonymous = false }
