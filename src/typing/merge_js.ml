(**
 * Copyright (c) 2014-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(* Connect the builtins object in master_cx to the builtins reference in some
   arbitrary cx. *)
let implicit_require_strict cx master_cx cx_to =
  let from_t = Flow_js.lookup_module master_cx Files.lib_module in
  let to_t = Flow_js.lookup_module cx_to Files.lib_module in
  Flow_js.flow_t cx (from_t, to_t)

(* Connect the export of cx_from to its import in cx_to. This happens in some
   arbitrary cx, so cx_from and cx_to should have already been copied to cx. *)
let explicit_impl_require_strict cx (cx_from, r, resolved_r, cx_to) =
  let resolved_r = Modulename.to_string resolved_r in
  let from_t = Flow_js.lookup_module cx_from resolved_r in
  let to_t = Flow_js.lookup_module cx_to r in
  Flow_js.flow_t cx (from_t, to_t)

(* Create the export of a resource file on the fly and connect it to its import
   in cxs_to. This happens in some arbitrary cx, so cx_to should have already
   been copied to cx. *)
let explicit_res_require_strict cx (r, f, cx_to) =
  let loc = SMap.find_unsafe r (Context.require_loc cx_to) in
  (* Recall that a resource file is not parsed, so its export doesn't depend on
     its contents, just its extension. So, we create the export of a resource
     file on the fly by looking at its extension. The general alternative of
     writing / reading these exports to / from separate contexts that live in a
     shared heap takes more time / space (linear in the number of resource
     files), so we avoid it. This optimization is analogous to what we do for
     unchecked files: we create the export (`any`) on the fly instead of writing
     / reading it to / from the context of each unchecked file. *)
  let from_t = Import_export.mk_resource_module_t cx loc f in
  let to_t = Flow_js.lookup_module cx_to r in
  Flow_js.flow_t cx (from_t, to_t)

(* Connect a export of a declared module to its import in cxs_to. This happens
   in some arbitrary cx, so cx_to should have already been copied to cx. *)
let explicit_decl_require_strict cx (m, resolved_m, cx_to) =
  let loc = SMap.find_unsafe m (Context.require_loc cx_to) in
  let reason = Reason.(mk_reason (RCustom m) loc) in

  (* lookup module declaration from builtin context *)
  (* TODO: cache in modulemap *)

  let m_name =
    resolved_m
    |> Modulename.to_string
    |> Reason.internal_module_name
  in
  let from_t = Flow_js.mk_tvar cx reason in
  Flow_js.lookup_builtin cx m_name reason
    (Type.NonstrictReturning (Some (Type.AnyT reason, from_t))) from_t;

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
   component_cxs and dep_cxs, resources contain the dependency edges between
   contexts in component_cxs and resource files, and declarations contain the
   dependency edges from component_cxs to master_cx.

   We assume that the first context in component_cxs is that of the leader (cx):
   this serves as the "host" for the merging. Let the remaining contexts in
   component_cxs be other_cxs.

   1. Copy dep_cxs, other_cxs, and master_cx to the host cx.

   2. Link the edges in implementations.

   3. Link the edges in resources.

   4. Link the edges in declarations.

   5. Link the local references to libraries in master_cx and component_cxs.
*)
let merge_component_strict component_cxs dep_cxs
    implementations resources declarations master_cx =
  let cx, other_cxs = List.hd component_cxs, List.tl component_cxs in
  Flow_js.Cache.clear();

  dep_cxs |> List.iter (Context.merge_into cx);
  other_cxs |> List.iter (Context.merge_into cx);
  Context.merge_into cx master_cx;

  implementations |> List.iter (explicit_impl_require_strict cx);

  resources |> List.iter (explicit_res_require_strict cx);

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


let merge_lib_file cx master_cx save_errors save_suppressions =
  Context.merge_into master_cx cx;
  implicit_require_strict master_cx master_cx cx;

  let errs = Context.errors cx in
  Context.remove_all_errors cx;
  save_errors (Context.file cx) errs;
  save_suppressions (Context.file cx) (Context.error_suppressions cx);

  ()


(****************** signature contexts *********************)

(* Once a context is merged with other contexts it depends on, it is ready to be
   optimized for use by contexts that in turn depend on it. The only interesting
   part of the context for such use is its exports. Thus, we call a context
   optimized for such use a "signature context."

   A signature context should contain only descriptions of its exports. Anything
   that does not contribute to those descriptions are redundant and should be
   discarded: otherwise, we would end up paying significant hidden costs, in
   terms of memory (storing in the heap) and time (serializing / deserializing
   from the heap, garbage collection pauses).

   As it turns out, it is not always possible or even desirable to describe the
   exports in "closed form" as types: instead, the descriptions take the form of
   a (mutually) recursive system of "equations" relating type variables to
   "solved" types, and the types themselves may contain these type variables
   inside. As indicated above, such descriptions are both necessary (due to
   recursive types) and desirable (due to sharing, and for cyclically depending
   contexts, due to the need to deal with the exports of all such contexts at
   once). This form is quite adequate for type checking (indeed, it can be
   thought of as merely a condensed form of usual contexts, which already
   exhibit this "interlinked" behavior).

   Unsurprisingly, since types also make references to some other maps in a
   context (property maps, envs), these references need to be carried around
   as well for the descriptions to be complete.

   We can collect compact and complete descriptions of exports by using a custom
   type visitor that marks relevant things that need to be carried over as it
   walks the context from its exports. Once the walk is done, we can replace the
   corresponding parts of the context (graph, property maps, envs) by their
   reduced forms.
*)
module ContextOptimizer = struct
  open Constraint
  open Type

  type quotient = {
    reduced_graph : node IMap.t;
    reduced_property_maps : Properties.map;
    reduced_export_maps : Exports.map;
    reduced_envs : Context.env IMap.t
  }

  let empty = {
    reduced_graph = IMap.empty;
    reduced_property_maps = Properties.Map.empty;
    reduced_export_maps = Exports.Map.empty;
    reduced_envs = IMap.empty;
  }

  class context_optimizer = object
    inherit [quotient] Type_visitor.t as super

    method! tvar cx quotient r id =
      let { reduced_graph; _ } = quotient in
      if (IMap.mem id reduced_graph) then quotient
      else
        let types = Flow_js.possible_types cx id in
        let t = match types with
          | [] -> AnyT.t
          | [t] -> t
          | t0::t1::ts -> UnionT (r, UnionRep.make t0 t1 ts)
        in
        let node = Root { rank = 0; constraints = Resolved t } in
        let reduced_graph = IMap.add id node reduced_graph in
        super#type_ cx { quotient with reduced_graph } t

    method! props cx quotient id =
      let { reduced_property_maps; _ } = quotient in
      if (Properties.Map.mem id reduced_property_maps) then quotient
      else
        let pmap = Context.find_props cx id in
        let reduced_property_maps =
          Properties.Map.add id pmap reduced_property_maps in
        super#props cx { quotient with reduced_property_maps } id

    method! exports cx quotient id =
      let { reduced_export_maps; _ } = quotient in
      if (Exports.Map.mem id reduced_export_maps) then quotient
      else
        let pmap = Context.find_exports cx id in
        let reduced_export_maps =
          Exports.Map.add id pmap reduced_export_maps in
        super#exports cx { quotient with reduced_export_maps } id

    method! fun_type cx quotient funtype =
      let id = funtype.closure_t in
      if id = 0 then super#fun_type cx quotient funtype
      else
        let { reduced_envs; _ } = quotient in
        let closure = IMap.find_unsafe id (Context.envs cx) in
        let reduced_envs = IMap.add id closure reduced_envs in
        super#fun_type cx { quotient with reduced_envs } funtype

  end

  (* walk a context from a list of exports *)
  let reduce_context cx exports =
    let reducer = new context_optimizer in
    List.fold_left (reducer#type_ cx) empty exports

  (* string form of a context's own module name *)
  let context_module cx =
    Modulename.to_string (Context.module_name cx)

  (* the tvar on which a context hosts its own exports *)
  let export cx =
    Flow_js.lookup_module cx (context_module cx)

  (* reduce a context to a "signature context" *)
  let sig_context component_cxs =
    let cx, other_cxs = List.hd component_cxs, List.tl component_cxs in
    let exports = List.map export component_cxs in
    let quotient = reduce_context cx exports in
    Context.set_graph cx quotient.reduced_graph;
    Context.set_property_maps cx quotient.reduced_property_maps;
    Context.set_export_maps cx quotient.reduced_export_maps;
    Context.set_envs cx quotient.reduced_envs;
    Context.set_type_graph cx (
      Graph_explorer.new_graph
        (IMap.fold (fun k _ -> ISet.add k) quotient.reduced_graph ISet.empty)
    );
    other_cxs |> List.iter (fun other_cx ->
      let other_m = context_module other_cx in
      Context.add_module cx other_m (export other_cx)
    )

end
