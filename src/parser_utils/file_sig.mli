(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* In Flow, every file creates a single module, but may also include declared
 * modules. This data structure describes all such modules.
 *
 * If a declared module with the same name appears twice, the last one will be
 * represented here.
 *
 * This representation is a bit broad, because implementation files generally
 * should not contain declare modules and declaration files (libdefs) are all
 * coalesced into a single module (builtins). *)
type t = {
  module_sig: module_sig;
  declare_modules: (Loc.t * module_sig) SMap.t
}

(* We can extract the observable interface of a module by extracting information
 * about what it requires and what it exports. *)
and module_sig = {
  requires: require SMap.t;
  module_kind: module_kind;
  type_exports_named: type_export SMap.t; (* export type {A, B as C} [from x] *)
  type_exports_star: export_star SMap.t; (* export type * from "foo" *)
}

(* We track information about dependencies for each unique module reference in a
 * file. For example, `import X from "foo"` and `require("foo")` both induce
 * dependencies on the same module and have the same module ref.
 *
 * Note that different refs can point to the same module, but we haven't
 * resolved modules yet, so we don't know where the ref actually points.
 *)
and require = {
  (* location of the last require/import *)
  loc: Loc.t;

  (* locations of every require *)
  cjs_requires: Loc.t list;

  (* locations of every import (incl. export from) *)
  es_imports: Loc.t list;

  (* map from remote name to local names of value imports
   * source: import {A, B as C} from "foo";
   * result: {A:{A:{[loc]}}, B:{C:{[loc]}}} *)
  named: Loc.t Nel.t SMap.t SMap.t;

  (* map from local name to location of namespace imports
   * source: import * as X from "foo";
   * result: {X:[loc]} *)
  ns: Loc.t Nel.t SMap.t;

  (* map from remote name to local names of type imports
   * source: import type {A, B as C} from "foo";
   * source: import {type A, type B as C} from "foo";
   * result: {A:{A:{[loc]}}, B:{C:{[loc]}}} *)
  types: Loc.t Nel.t SMap.t SMap.t;

  (* map from remote name to local names of typeof imports
   * source: import typeof {A, B as C} from "foo";
   * source: import {typeof A, typeof B as C} from "foo";
   * result: {A:{A:{[loc]}}, B:{C:{[loc]}}} *)
  typesof: Loc.t Nel.t SMap.t SMap.t;

  (* map from local name to location of namespace typeof imports
   * source: import typeof * as X from "foo";
   * result: {X:[loc]} *)
  typesof_ns: Loc.t Nel.t SMap.t;
}

(* All modules are assumed to be CommonJS to start with, but if we see an ES
 * module-style import or export, we switch to ES *)
and module_kind =
  | CommonJS of {
    (* loc of assignment to module.exports *)
    clobbered: Loc.t option
  }
  | ES of {
    (* map from exported name to export data *)
    named: export SMap.t;
    (* map from module reference to location of `export *` *)
    star: export_star SMap.t
  }

and export =
  | ExportDefault of {
    (* may have local name, e.g., `export default function foo {}` *)
    local: (Loc.t * string) option
  }
  | ExportNamed of {
    (* loc of remote name *)
    loc: Loc.t;
    (* may have local name, e.g., `export {foo as bar}` *)
    local: (Loc.t * string) option;
    (* module reference for re-exports, e.g., `export {foo} from 'bar'` *)
    source: (Loc.t * string) option
  }
  | ExportNs of {
    (* loc of remote name *)
    loc: Loc.t;
    (* module reference of exported namespace *)
    source: Loc.t * string
  }

and export_star =
  | ExportStar of { star_loc: Loc.t; source_loc: Loc.t }

and type_export =
  | TypeExportNamed of {
    (* loc of remote name *)
    loc: Loc.t;
    (* may have local name, e.g., `export type {T as U}` *)
    local: (Loc.t * string) option;
    (* module reference for re-exports, e.g., `export {T} from 'bar'` *)
    source: (Loc.t * string) option
  }

val empty_file_sig: t
val empty_module_sig: module_sig

val require_loc_map: module_sig -> Loc.t Nel.t SMap.t

val program: ast:Loc.t Ast.program -> t
