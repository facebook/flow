(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type_sig
open Type_sig_collections
module P = Type_sig_pack

type node = Cycle_hash.node

type read_hash = Cycle_hash.read_hash

type 'a checked_dep =
  | CJS of {
      filename: read_hash;
      ns: 'a;
      type_exports: 'a SMap.t;
      exports: 'a option;
    }
  | ES of {
      filename: read_hash;
      ns: 'a;
      type_exports: 'a SMap.t;
      exports: 'a SMap.t;
    }

type dependency =
  | Cyclic of node checked_dep Lazy.t
  | Acyclic of read_hash checked_dep Lazy.t
  | Resource of read_hash
  | Unchecked

type file = {
  dependencies: dependency Module_refs.t;
  local_defs: node Local_defs.t;
  remote_refs: node Remote_refs.t;
  pattern_defs: node Pattern_defs.t;
  patterns: node Patterns.t;
}

let require edge dep_edge = function
  | CJS { filename; exports; _ } ->
    dep_edge filename;
    Option.iter edge exports
  | ES { filename; ns; _ } ->
    dep_edge filename;
    edge ns

let import name edge dep_edge = function
  | CJS { filename; ns; type_exports; exports } ->
    dep_edge filename;
    (* If we `import type` from a CJS module, the module's exports object might
     * override any named type export. For example:
     *   > export type T = ...
     *   > module.exports = { T: class {} }
     * For hashing, we conservatively edge to both the exports object as well as
     * any reachable named exports, including those reachable via star exports *)
    Option.iter edge exports;
    begin
      match SMap.find_opt name type_exports with
      | Some node -> edge node
      | None ->
        (* The ns object changes if any star exports change. *)
        edge ns
    end
  | ES { filename; ns; type_exports; exports } ->
    dep_edge filename;
    (* Note: type_exports and exports are disjoint. *)
    (match SMap.find_opt name type_exports with
    | Some node -> edge node
    | None ->
      (match SMap.find_opt name exports with
      | Some node -> edge node
      | None ->
        (* If this module does not export the name directly, it might still
         * export indirectly. The ns object changes if any star exports change. *)
        edge ns))

let import_ns edge dep_edge = function
  | CJS { filename; ns; _ }
  | ES { filename; ns; _ } ->
    dep_edge filename;
    edge ns

let edge_local_def edge file index = edge (Local_defs.get file.local_defs index)

let edge_remote_ref edge file index = edge (Remote_refs.get file.remote_refs index)

let edge_pattern edge file index = edge (Patterns.get file.patterns index)

let edge_pattern_def edge file index = edge (Pattern_defs.get file.pattern_defs index)

let edge_require edge dep_edge file index =
  match Module_refs.get file.dependencies index with
  | Cyclic (lazy dep) -> require edge dep_edge dep
  | Acyclic (lazy dep) -> require dep_edge dep_edge dep
  | Resource dep -> dep_edge dep
  | Unchecked ->
    (* All unchecked dependencies are equivalent. If a dependency changes from
     * unchecked to checked, then the hash will change anyway. *)
    ()

let edge_import name edge dep_edge file index =
  match Module_refs.get file.dependencies index with
  | Cyclic (lazy dep) -> import name edge dep_edge dep
  | Acyclic (lazy dep) -> import name dep_edge dep_edge dep
  | Resource dep -> dep_edge dep
  | Unchecked ->
    (* All unchecked dependencies are equivalent. If a dependency changes from
     * unchecked to checked, then the hash will change anyway. *)
    ()

let edge_import_ns edge dep_edge file index =
  match Module_refs.get file.dependencies index with
  | Cyclic (lazy dep) -> import_ns edge dep_edge dep
  | Acyclic (lazy dep) -> import_ns dep_edge dep_edge dep
  | Resource dep -> dep_edge dep
  | Unchecked ->
    (* All unchecked dependencies are equivalent. If a dependency changes from
     * unchecked to checked, then the hash will change anyway. *)
    ()

let visit_ref edge file = function
  | P.LocalRef { ref_loc = _; index } -> edge_local_def edge file index
  | P.RemoteRef { ref_loc = _; index } -> edge_remote_ref edge file index
  | P.BuiltinRef { ref_loc = _; name = _ } ->
    (* If the builtins change, we will restart anyway, so there is no need to do
     * any hashing here. *)
    ()

let rec visit_tyref edge dep_edge file = function
  | P.Unqualified ref -> visit_ref edge file ref
  | P.Qualified { loc = _; id_loc = _; name = _; qualification } ->
    visit_tyref edge dep_edge file qualification

let rec visit_packed edge dep_edge file = function
  | P.Annot t -> visit_annot edge dep_edge file t
  | P.Value t -> visit_value edge dep_edge file t
  | P.Ref ref -> visit_ref edge file ref
  | P.TyRef ref -> visit_tyref edge dep_edge file ref
  | P.TyRefApp { loc = _; name; targs } ->
    visit_tyref edge dep_edge file name;
    List.iter (visit_packed edge dep_edge file) targs
  | P.AsyncVoidReturn loc -> ignore (loc : Locs.index)
  | P.Pattern index -> edge_pattern edge file index
  | P.Err loc -> ignore (loc : Locs.index)
  | P.Eval (loc, t, op) ->
    ignore (loc : Locs.index);
    visit_eval edge dep_edge file t op
  | P.Require { loc = _; index } -> edge_require edge dep_edge file index
  | P.ImportDynamic { loc = _; index } -> edge_import_ns edge dep_edge file index
  | P.ModuleRef { loc = _; index } -> edge_require edge dep_edge file index

and visit_eval edge dep_edge file t op =
  visit_packed edge dep_edge file t;
  visit_op edge dep_edge file op

and visit_annot edge dep_edge file t = iter_annot ignore (visit_packed edge dep_edge file) t

and visit_value edge dep_edge file t = iter_value ignore (visit_packed edge dep_edge file) t

and visit_op edge dep_edge file op = iter_op (visit_packed edge dep_edge file) op

let visit_def edge dep_edge file def = iter_def ignore (visit_packed edge dep_edge file) def

let visit_remote_ref edge dep_edge file = function
  | P.Import { index; remote; _ }
  | P.ImportType { index; remote; _ }
  | P.ImportTypeof { index; remote; _ } ->
    edge_import remote edge dep_edge file index
  | P.ImportTypeofNs { index; _ }
  | P.ImportNs { index; _ } ->
    edge_import_ns edge dep_edge file index

let visit_pattern edge file = function
  | P.PDef index -> edge_pattern_def edge file index
  | P.ComputedP { elem; def } ->
    edge_pattern_def edge file elem;
    edge_pattern edge file def
  | P.PropP { id_loc = _; name = _; def; _ }
  | P.ObjRestP { loc = _; xs = _; def }
  | P.IndexP { loc = _; i = _; def }
  | P.ArrRestP { loc = _; i = _; def } ->
    edge_pattern edge file def
  | P.UnsupportedLiteralP loc -> ignore (loc : Locs.index)

let visit_export edge dep_edge file = function
  | P.ExportRef ref -> visit_ref edge file ref
  | P.ExportDefault { default_loc = _; def } -> visit_packed edge dep_edge file def
  | P.ExportBinding index
  | P.ExportDefaultBinding { default_loc = _; index } ->
    edge_local_def edge file index
  | P.ExportFrom index -> edge_remote_ref edge file index

let visit_type_export edge file = function
  | P.ExportTypeRef ref -> visit_ref edge file ref
  | P.ExportTypeBinding index -> edge_local_def edge file index
  | P.ExportTypeFrom index -> edge_remote_ref edge file index
