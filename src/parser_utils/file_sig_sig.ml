(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  module L : Loc_sig.S

  (* In Flow, every file creates a single module, but may also include declared
   * modules. This data structure describes all such modules.
   *
   * If a declared module with the same name appears twice, the last one will be
   * represented here.
   *
   * This representation is a bit broad, because implementation files generally
   * should not contain declare modules and declaration files (libdefs) are all
   * coalesced into a single module (builtins). *)
  type 'info t' = {
    module_sig: 'info module_sig';
    declare_modules: (L.t * 'info module_sig') SMap.t;
  }

  (* We can extract the observable interface of a module by extracting information
   * about what it requires and what it exports. *)
  and 'info module_sig' = {
    requires: require list;
    module_kind: module_kind;
    type_exports_named: (string * (L.t * type_export)) list;
    (* export type {A, B as C} [from x] *)
    type_exports_star: (L.t * export_star) list;
    (* export type * from "foo" *)
    info: 'info; (* useful to carry information that might eventually be erased *)
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
        source: L.t Flow_ast_utils.source;
        require_loc: L.t;
        (* Note: These are best-effort.
         * DO NOT use these for typechecking. *)
        bindings: require_bindings option;
      }
    (* import('foo').then(...) *)
    | ImportDynamic of {
        source: L.t Flow_ast_utils.source;
        import_loc: L.t;
      }
    (* import declaration without specifiers
     *
     * Note that this is equivalent to the Import variant below with all fields
     * empty, but modeled as a separate variant to ensure use sites handle this
     * case if necessary. *)
    | Import0 of { source: L.t Flow_ast_utils.source }
    (* import declaration with specifiers *)
    | Import of {
        import_loc: L.t;
        (* location of module ref *)
        source: L.t Flow_ast_utils.source;
        (* map from remote name to local names of value imports
         * source: import {A, B as C} from "foo";
         * result: {A:{A:{[ImportedLocs {_}]}}, B:{C:{[ImportedLocs {_}]}}}
         *
         * Multiple locations for a given (remoteName, localName) pair are not typical, but they can
         * occur e.g. with the code `import {foo, foo} from 'bar';`. This code would cause an error
         * later because of the duplicate local name, but we should handle it here since it does parse.
         *)
        named: imported_locs Nel.t SMap.t SMap.t;
        (* optional pair of location of namespace import and local name
         * source: import * as X from "foo";
         * result: loc, X *)
        ns: L.t Flow_ast_utils.ident option;
        (* map from remote name to local names of type imports
         * source: import type {A, B as C} from "foo";
         * source: import {type A, type B as C} from "foo";
         * result: {A:{A:{[ImportedLocs {_}]}}, B:{C:{[ImportedLocs {_}]}}} *)
        types: imported_locs Nel.t SMap.t SMap.t;
        (* map from remote name to local names of typeof imports
         * source: import typeof {A, B as C} from "foo";
         * source: import {typeof A, typeof B as C} from "foo";
         * result: {A:{A:{[ImportedLocs {_}]}}, B:{C:{[ImportedLocs {_}]}}} *)
        typesof: imported_locs Nel.t SMap.t SMap.t;
        (* optional pair of location of namespace typeof import and local name
         * source: import typeof * as X from "foo";
         * result: loc, X *)
        typesof_ns: L.t Flow_ast_utils.ident option;
      }

  and imported_locs = {
    remote_loc: L.t;
    local_loc: L.t;
  }

  and require_bindings =
    (* source: const bar = require('./foo');
     * result: bar *)
    | BindIdent of L.t Flow_ast_utils.ident
    (* map from remote name to local names of requires
     * source: const {a, b: c} = require('./foo');
     * result: {a: (a_loc, a), b: (c_loc, c)} *)
    | BindNamed of (L.t Flow_ast_utils.ident * require_bindings) list

  (* All modules are assumed to be CommonJS to start with, but if we see an ES
   * module-style export, we switch to ES. *)
  and module_kind =
    | CommonJS of { mod_exp_loc: L.t option }
    | ES of {
        (* map from exported name to export data *)
        named: (string * (L.t * export)) list;
        (* map from module reference to location of `export *` *)
        star: (L.t * export_star) list;
      }

  and export =
    | ExportDefault of {
        (* location of the `default` keyword *)
        default_loc: L.t;
            (** NOTE: local = Some id if and only if id introduces a local binding **)
        (* may have local name, e.g., `export default function foo {}` *)
        local: L.t Flow_ast_utils.ident option;
      }
    | ExportNamed of {
        (* loc of remote name *)
        loc: L.t;
        kind: named_export_kind;
      }
    | ExportNs of {
        (* loc of remote name *)
        loc: L.t;
        star_loc: L.t;
        (* module reference of exported namespace *)
        source: L.t Flow_ast_utils.source;
      }

  and named_export_kind =
    | NamedDeclaration
    | NamedSpecifier of {
        (* local name, e.g., `export {foo as bar}`, `export type {T as U}` *)
        local: L.t Flow_ast_utils.ident;
        (* module reference for re-exports, e.g., `export {foo} from 'bar'`,`export type {T} from 'bar'` *)
        source: L.t Flow_ast_utils.source option;
      }

  and export_star =
    | ExportStar of {
        star_loc: L.t;
        source: L.t Flow_ast_utils.source;
      }

  and type_export =
    | TypeExportNamed of {
        (* loc of remote name *)
        loc: L.t;
        kind: named_export_kind;
      }
  [@@deriving show]

  type tolerable_error =
    (* e.g. `module.exports.foo = 4` when not at the top level *)
    | BadExportPosition of L.t
    (* e.g. `foo(module)`, dangerous because `module` is aliased *)
    | BadExportContext of string (* offending identifier *) * L.t
    | SignatureVerificationError of L.t Signature_error.t
  [@@deriving show]

  type exports_info = {
    module_kind_info: module_kind_info;
    type_exports_named_info: es_export_def list;
  }

  and module_kind_info =
    | CommonJSInfo of cjs_exports_def list
    | ESInfo of es_export_def list

  and cjs_exports_def =
    | DeclareModuleExportsDef of (L.t, L.t) Flow_ast.Type.annotation
    | SetModuleExportsDef of (L.t, L.t) Flow_ast.Expression.t
    | AddModuleExportsDef of L.t Flow_ast_utils.ident * (L.t, L.t) Flow_ast.Expression.t

  and es_export_def =
    | DeclareExportDef of (L.t, L.t) Flow_ast.Statement.DeclareExportDeclaration.declaration
    | ExportDefaultDef of (L.t, L.t) Flow_ast.Statement.ExportDefaultDeclaration.declaration
    | ExportNamedDef of (L.t, L.t) Flow_ast.Statement.t
  [@@deriving show]

  type error = IndeterminateModuleType of L.t

  type exports_t = exports_info t' [@@deriving show]

  val program_with_exports_info :
    ast:(L.t, L.t) Flow_ast.Program.t ->
    module_ref_prefix:string option ->
    (exports_t * tolerable_error list, error) result

  (* Use for debugging; not for exposing info to the end user *)
  val exports_info_to_string : exports_info -> string

  (* Applications may not care about the info carried by signatures. *)
  type module_sig = unit module_sig' [@@deriving show]

  type t = unit t' [@@deriving show]

  val init : t

  val program :
    ast:(L.t, L.t) Flow_ast.Program.t ->
    module_ref_prefix:string option ->
    (t * tolerable_error list, error) result

  val map_unit_file_sig : exports_info t' -> t

  (* Use for debugging; not for exposing info to the end user *)
  val to_string : t -> string

  val require_loc_map : module_sig -> L.t Nel.t SMap.t

  (* Only the keys returned by `require_loc_map` *)
  val require_set : module_sig -> SSet.t

  class mapper :
    object
      method error : error -> error

      method export : string * (L.t * export) -> string * (L.t * export)

      method export_star : L.t * export_star -> L.t * export_star

      method file_sig : t -> t

      method ident : L.t Flow_ast_utils.ident -> L.t Flow_ast_utils.ident

      method source : L.t Flow_ast_utils.source -> L.t Flow_ast_utils.source

      method named_export_kind : named_export_kind -> named_export_kind

      method imported_locs : imported_locs -> imported_locs

      method loc : L.t -> L.t

      method module_kind : module_kind -> module_kind

      method module_sig : module_sig -> module_sig

      method require : require -> require

      method require_bindings : require_bindings -> require_bindings

      method tolerable_error : tolerable_error -> tolerable_error

      method type_export : string * (L.t * type_export) -> string * (L.t * type_export)
    end
end
