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
  declare_modules: (Loc.t * module_sig) SMap.t;
  tolerable_errors: tolerable_error list;
}

(* We can extract the observable interface of a module by extracting information
 * about what it requires and what it exports. *)
and module_sig = {
  requires: require list;
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
and require =
  (* require('foo'); *)
  | Require of {
    (* location of module ref *)
    source: ident;

    require_loc: Loc.t;

    (* Note: These are best-effort.
     * DO NOT use these for typechecking. *)
    bindings: require_bindings option;

  }

  (* import('foo').then(...) *)
  | ImportDynamic of { source: ident; import_loc: Loc.t }

  (* import declaration without specifiers
   *
   * Note that this is equivalent to the Import variant below with all fields
   * empty, but modeled as a separate variant to ensure use sites handle this
   * case if necessary. *)
  | Import0 of ident

  (* import declaration with specifiers *)
  | Import of {
    (* location of module ref *)
    source: ident;

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

and require_bindings =
  (* source: const bar = require('./foo');
   * result: bar *)
  | BindIdent of ident
  (* map from local name to (local_loc, remote name)
   * source: const {a, b: c} = require('./foo');
   * result: {a: (a_loc, a), c: (c_loc, b)} *)
  | BindNamed of (Loc.t * ident) SMap.t

(* All modules are assumed to be CommonJS to start with, but if we see an ES
 * module-style export, we switch to ES. *)
and module_kind =
  (* The CJS interface is still experimental.
   * DO NOT use this for typechecking. *)
  | CommonJS of {
    exports: cjs_exports option;
  }
  | ES of {
    (* map from exported name to export data *)
    named: export SMap.t;
    (* map from module reference to location of `export *` *)
    star: export_star SMap.t;
  }

and cjs_exports =
  | CJSExportIdent of ident
    (* map from exported name to export data *)
  | CJSExportProps of cjs_export SMap.t
  | CJSExportOther

and cjs_export =
  | CJSExport of {
    (* location of exported name *)
    loc: Loc.t;
    (* may have local name *)
    local: ident option;
  }

and export =
  | ExportDefault of {
    (* location of the `default` keyword *)
    default_loc: Loc.t;
    (* may have local name, e.g., `export default function foo {}` *)
    local: ident option;
  }
  | ExportNamed of {
    (* loc of remote name *)
    loc: Loc.t;
    (* may have local name, e.g., `export {foo as bar}` *)
    local: ident option;
    (* module reference for re-exports, e.g., `export {foo} from 'bar'` *)
    source: ident option;
  }
  | ExportNs of {
    (* loc of remote name *)
    loc: Loc.t;
    (* module reference of exported namespace *)
    source: ident;
  }

and export_star =
  | ExportStar of { star_loc: Loc.t; source_loc: Loc.t }

and type_export =
  | TypeExportNamed of {
    (* loc of remote name *)
    loc: Loc.t;
    (* may have local name, e.g., `export type {T as U}` *)
    local: ident option;
    (* module reference for re-exports, e.g., `export {T} from 'bar'` *)
    source: ident option;
  }

and ident = Loc.t * string

and tolerable_error =
  | BadExportPosition of Loc.t

type error =
  | IndeterminateModuleType of Loc.t

val empty_file_sig: t
val empty_module_sig: module_sig

val require_loc_map: module_sig -> Loc.t Nel.t SMap.t

val program: ast:Loc.t Ast.program -> (t, error) result
