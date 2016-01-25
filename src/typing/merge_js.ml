(**
 * Copyright (c) 2014-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Utils

(* Connect the builtins object in master_cx to the builtins reference in some
   arbitrary cx. *)
let implicit_require_strict cx master_cx cx_to =
  let from_t = Flow_js.lookup_module master_cx Files_js.lib_module in
  let to_t = Flow_js.lookup_module cx_to Files_js.lib_module in
  Flow_js.flow_t cx (from_t, to_t)

(* Connect the export of cx_from to its import in cx_to. This happens in some
   arbitrary cx, so cx_from and cx_to should have already been copied to cx. *)
let explicit_impl_require_strict cx (cx_from, r, resolved_r, cx_to) =
  let resolved_r = Modulename.to_string resolved_r in
  let from_t = Flow_js.lookup_module cx_from resolved_r in
  let to_t = Flow_js.lookup_module cx_to r in
  Flow_js.flow_t cx (from_t, to_t)

(* Connect a export of a declared module to its import in cxs_to. This happens
   in some arbitrary cx, so cx_to should have already been copied to cx. *)
let explicit_decl_require_strict cx (m, resolved_m, cx_to) =
  let loc = SMap.find_unsafe m (Context.require_loc cx_to) in
  let reason = Reason_js.mk_reason m loc in

  (* lookup module declaration from builtin context *)
  (* TODO: cache in modulemap *)

  let m_name =
    resolved_m
    |> Modulename.to_string
    |> Reason_js.internal_module_name
  in
  let from_t = Flow_js.mk_tvar cx reason in
  Flow_js.lookup_builtin cx m_name reason None from_t;

  (* flow the declared module type to importing context *)
  let to_t = Flow_js.lookup_module cx_to m in
  Flow_js.flow_t cx (from_t, to_t)


(* Merge a component with its "implicit requires" and "explicit requires." The
   implicit requires are those defined in libraries. For the explicit
   requires, we need to merge only those parts of the dependency graph that the
   component immediately depends on. (We assume that this merging is part of a
   recursive process that has already handled recursive dependencies.)

   Now, by definition, files in a component can bidirectionally depend only on
   other files in the component. All other dependencies are unidirectional.

   Let dep_cxs contain the (optimized) contexts of all dependencies that are
   unidirectional, and let component_cxs contain the contexts of the files in
   the component. Let master_cx be the (optimized) context of libraries.

   Let implementations contain the dependency edges between contexts in
   component_cxs and dep_cxs, and declarations contain the dependency edges from
   component_cxs to master_cx.

   We assume that the first context in component_cxs is that of the leader (cx):
   this serves as the "host" for the merging. Let the remaining contexts in
   component_cxs be other_cxs.

   1. Copy dep_cxs, other_cxs, and master_cx to the host cx.

   2. Link the edges in implementations.

   3. Link the edges in declarations.

   4. Link the local references to libraries in master_cx and component_cxs.
*)
let merge_component_strict component_cxs dep_cxs
    implementations declarations master_cx =
  let cx, other_cxs = List.hd component_cxs, List.tl component_cxs in
  Flow_js.Cache.clear();

  dep_cxs |> List.iter (Context.merge_into cx);
  other_cxs |> List.iter (Context.merge_into cx);
  Context.merge_into cx master_cx;

  implementations |> List.iter (explicit_impl_require_strict cx);

  declarations |> List.iter (explicit_decl_require_strict cx);

  other_cxs |> List.iter (implicit_require_strict cx master_cx);
  implicit_require_strict cx master_cx cx;

  ()

(* After merging dependencies into a context (but before optimizing the
   context), it is important to restore the parts of the context that were
   copied from other, already optimized contexts (dep_cxs and master_cx, see
   above comment for details on what they mean). Indeed, merging is an
   imperative process, and there is no guarantee that those parts of the context
   would have remained unchanged.

   Restoration maintains consistency for "diamond-shaped" dependency relations:
   it forces two contexts B and C that depend on the same context A to agree on
   the meaning of the parts of A they share (and that meaning is dictated by A
   itself), and so some context D that depends on both B and C (and perhaps A
   too) is never confused when merging them.
*)
let restore cx dep_cxs master_cx =
  dep_cxs |> List.iter (Context.merge_into cx);
  Context.merge_into cx master_cx


let merge_lib_file cx save_errors save_suppressions =
  let master_cx = Flow_js.master_cx () in
  Context.merge_into master_cx cx;
  implicit_require_strict master_cx master_cx cx;

  let errs = Context.errors cx in
  Context.remove_all_errors cx;
  save_errors (Context.file cx) errs;
  save_suppressions (Context.file cx) (Context.error_suppressions cx);

  ()
