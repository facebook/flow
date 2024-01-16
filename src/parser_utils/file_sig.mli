(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type options = {
  enable_enums: bool;
  enable_relay_integration: bool;
  explicit_available_platforms: string list option;
  file_options: Files.options;
  haste_module_ref_prefix: string option;
  haste_module_ref_prefix_LEGACY_INTEROP: string option;
  relay_integration_module_prefix: string option;
}

(* We can extract the observable interface of a module by extracting information
 * about what it requires and what it exports. *)
type t

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
      source: Loc.t Flow_ast_utils.source;
      require_loc: Loc.t;
      (* Note: These are best-effort.
       * DO NOT use these for typechecking. *)
      bindings: require_bindings option;
      prefix: string option;
    }
  (* import('foo').then(...) *)
  | ImportDynamic of {
      source: Loc.t Flow_ast_utils.source;
      import_loc: Loc.t;
    }
  (* import declaration without specifiers
   *
   * Note that this is equivalent to the Import variant below with all fields
   * empty, but modeled as a separate variant to ensure use sites handle this
   * case if necessary. *)
  | Import0 of { source: Loc.t Flow_ast_utils.source }
  (* A synthetic import declaration without a source location *)
  | ImportSynthetic of { source: string }
  (* import declaration with specifiers *)
  | Import of {
      import_loc: Loc.t;
      (* location of module ref *)
      source: Loc.t Flow_ast_utils.source;
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
      ns: Loc.t Flow_ast_utils.ident option;
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
      typesof_ns: Loc.t Flow_ast_utils.ident option;
    }
  | ExportFrom of { source: Loc.t Flow_ast_utils.source }

and imported_locs = {
  remote_loc: Loc.t;
  local_loc: Loc.t;
}

and require_bindings =
  (* source: const bar = require('./foo');
   * result: bar *)
  | BindIdent of Loc.t Flow_ast_utils.ident
  (* map from remote name to local names of requires
   * source: const {a, b: c} = require('./foo');
   * result: {a: (a_loc, a), b: (c_loc, c)} *)
  | BindNamed of (Loc.t Flow_ast_utils.ident * require_bindings) list
[@@deriving show]

type tolerable_error = SignatureVerificationError of Loc.t Signature_error.t [@@deriving show]

type tolerable_t = t * tolerable_error list

val empty : t

val default_opts : options

val program : file_key:File_key.t -> ast:(Loc.t, Loc.t) Flow_ast.Program.t -> opts:options -> t

(* Use for debugging; not for exposing info to the end user *)
val to_string : t -> string

val require_loc_map : t -> Loc.t list SMap.t

(* Only the keys returned by `require_loc_map` *)
val require_set : t -> SSet.t

val requires : t -> require list
