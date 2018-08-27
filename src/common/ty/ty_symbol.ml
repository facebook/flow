(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type provenance =
  | Local of Loc.t      (* Defined locally *)
  | Imported of Loc.t   (* Defined remotely, imported to file *)
  | Remote of Loc.t     (* Defined remotely, NOT imported to file *)
  | Library of Loc.t    (* Defined in library *)
  | Builtin

type identifier = string

type symbol = Symbol of (provenance * identifier) [@@unboxed]

let builtin_symbol name =
  Symbol (Builtin, name)
