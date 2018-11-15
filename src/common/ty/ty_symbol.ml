(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type provenance =
  | Local       (* Defined locally *)
  | Imported    (* Defined remotely, imported to file *)
  | Remote      (* Defined remotely, NOT imported to file *)
  | Library     (* Defined in library *)
  | Builtin

type symbol = {
  provenance: provenance;
  loc: ALoc.t;
  name: string;
  anonymous: bool;
}

let builtin_symbol name = {
  provenance = Builtin;
  loc = ALoc.none;
  name;
  anonymous = false;
}
