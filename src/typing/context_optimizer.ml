(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open TypeUtil
open Constraint

let merge_trust_var constr =
  Trust_constraint.(
    match constr with
    | TrustResolved t -> t
    | TrustUnresolved bound -> get_trust bound |> Trust.fix)

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

   NOTE: Changing ids of entities stored in the Context is dangerous here,
   since it would require changing every occurence of that id. As such, the mapper
   currently only traverses ids for side-effects, but returns the original id.

   There are also several places where we add something to a map, recurse, then
   readd. The original add makes sure we don't visit the same entity twice, the
   second actually changes the entity in the exports.
*)

class context_optimizer ~no_lowers =
  object (self)
    inherit [Polarity.t, Type.Constraint.infer_phase] Type_mapper.t_with_uses as super

    val sig_hash = Xx.init 0L

    method sig_hash () = Xx.digest sig_hash

    val mutable next_stable_id = 0

    method fresh_stable_id =
      let stable_id = next_stable_id in
      next_stable_id <- next_stable_id + 1;
      stable_id

    val mutable stable_tvar_ids = IMap.empty

    val mutable stable_trust_var_ids = IMap.empty

    val mutable stable_eval_ids = IMap.empty

    val mutable stable_poly_ids = IMap.empty

    val mutable stable_props_ids = IMap.empty

    val mutable stable_call_prop_ids = IMap.empty

    val mutable reduced_module_map = NameUtils.Map.empty

    val mutable reduced_graph : Type.Constraint.infer_phase Type.Constraint.node IMap.t = IMap.empty

    val mutable reduced_trust_graph = IMap.empty

    val mutable reduced_property_maps = Properties.Map.empty

    val mutable reduced_call_props = IMap.empty

    val mutable reduced_export_maps = Exports.Map.empty

    val mutable reduced_evaluated = Eval.Map.empty

    val mutable export_reason = None

    method reduce cx module_ref =
      let export = Context.find_module cx module_ref in
      let export' = self#type_ cx Polarity.Neutral export in
      reduced_module_map <-
        NameUtils.Map.add (Reason.OrdinaryName module_ref) export' reduced_module_map

    method tvar cx pole r id =
      let (root_id, _) = Context.find_constraints cx id in
      if id == root_id then (
        if IMap.mem id reduced_graph then (
          let stable_id = IMap.find root_id stable_tvar_ids in
          SigHash.add_int sig_hash stable_id;
          id
        ) else
          let t = Flow_js_utils.merge_tvar ~no_lowers cx r id in
          let node =
            Root { rank = 0; constraints = Lazy.from_val (FullyResolved (unknown_use, lazy t)) }
          in
          reduced_graph <- IMap.add id node reduced_graph;
          let () =
            let stable_id = self#fresh_stable_id in
            stable_tvar_ids <- IMap.add id stable_id stable_tvar_ids
          in
          let t = self#type_ cx pole t in
          let node =
            Root { rank = 0; constraints = Lazy.from_val (FullyResolved (unknown_use, lazy t)) }
          in
          reduced_graph <- IMap.add id node reduced_graph;
          id
      ) else (
        ignore (self#tvar cx pole r root_id);
        let node = Goto root_id in
        reduced_graph <- IMap.add id node reduced_graph;
        id
      )

    method trust_var cx pole id =
      let (root_id, constr) = Context.find_trust_constraints cx id in
      if id == root_id then (
        if IMap.mem id reduced_trust_graph then (
          let stable_id = IMap.find root_id stable_trust_var_ids in
          SigHash.add_int sig_hash stable_id;
          id
        ) else
          let t = merge_trust_var constr in
          let node = Trust_constraint.new_resolved_root t in
          reduced_trust_graph <- IMap.add id node reduced_trust_graph;
          let () =
            let stable_id = self#fresh_stable_id in
            stable_trust_var_ids <- IMap.add id stable_id stable_trust_var_ids
          in
          id
      ) else (
        ignore (self#trust_var cx pole root_id);
        let node = Trust_constraint.TrustGoto root_id in
        reduced_trust_graph <- IMap.add id node reduced_trust_graph;
        id
      )

    method props cx pole id =
      if Properties.Map.mem id reduced_property_maps then
        let () =
          Base.Option.iter
            ~f:(fun id_int ->
              let stable_id =
                if Context.mem_nominal_prop_id cx id_int then
                  IMap.find id_int stable_props_ids
                else
                  id_int
              in
              SigHash.add_int sig_hash stable_id)
            (Properties.id_as_int id)
        in
        id
      else
        let () =
          Base.Option.iter
            ~f:(fun id_int ->
              let stable_id =
                if Context.mem_nominal_prop_id cx id_int then (
                  let stable_id = self#fresh_stable_id in
                  stable_props_ids <- IMap.add id_int stable_id stable_props_ids;
                  stable_id
                ) else
                  id_int
              in
              SigHash.add_int sig_hash stable_id)
            (Properties.id_as_int id)
        in
        let pmap = Context.find_props cx id in
        let () = SigHash.add_props_map sig_hash pmap in
        reduced_property_maps <- Properties.Map.add id pmap reduced_property_maps;
        let pmap' = NameUtils.Map.ident_map (self#prop cx pole) pmap in
        reduced_property_maps <- Properties.Map.add id pmap' reduced_property_maps;
        id

    method call_prop cx pole id =
      if IMap.mem id reduced_call_props then
        let stable_id = IMap.find id stable_call_prop_ids in
        let () = SigHash.add_int sig_hash stable_id in
        id
      else
        let () =
          let stable_id = self#fresh_stable_id in
          stable_call_prop_ids <- IMap.add id stable_id stable_call_prop_ids
        in
        let t = Context.find_call cx id in
        reduced_call_props <- IMap.add id t reduced_call_props;
        let t' = self#type_ cx pole t in
        reduced_call_props <- IMap.add id t' reduced_call_props;
        id

    method! export_types cx map_cx e =
      let { exports_tmap; cjs_export; has_every_named_export } = e in
      let exports_tmap' = self#exports cx map_cx exports_tmap in
      let cjs_export' =
        OptionUtils.ident_map
          (fun exp ->
            export_reason <- Some (reason_of_t exp);
            self#type_ cx map_cx exp)
          cjs_export
      in
      if exports_tmap == exports_tmap' && cjs_export == cjs_export' then
        e
      else
        { exports_tmap = exports_tmap'; cjs_export = cjs_export'; has_every_named_export }

    method exports cx pole id =
      if Exports.Map.mem id reduced_export_maps then
        id
      else
        let tmap = Context.find_exports cx id in
        let map_pair p =
          let (loc, t) = p in
          export_reason <- Some (reason_of_t t);
          let t' = self#type_ cx pole t in
          if t == t' then
            p
          else
            (loc, t')
        in
        reduced_export_maps <- Exports.Map.add id tmap reduced_export_maps;
        let tmap' = NameUtils.Map.ident_map map_pair tmap in
        reduced_export_maps <- Exports.Map.add id tmap' reduced_export_maps;
        SigHash.add_exports_map sig_hash tmap';
        id

    method eval_id cx pole id =
      if Eval.Map.mem id reduced_evaluated then (
        Eval.id_as_int id
        |> Base.Option.iter ~f:(fun int_id ->
               IMap.find int_id stable_eval_ids |> SigHash.add_int sig_hash);
        id
      ) else
        let stable_id = self#fresh_stable_id in
        Eval.id_as_int id
        |> Base.Option.iter ~f:(fun int_id ->
               stable_eval_ids <- IMap.add int_id stable_id stable_eval_ids);
        match Eval.Map.find_opt id (Context.evaluated cx) with
        | None -> id
        | Some t ->
          reduced_evaluated <- Eval.Map.add id t reduced_evaluated;
          let t' = self#type_ cx pole t in
          reduced_evaluated <- Eval.Map.add id t' reduced_evaluated;
          id

    method! destructor cx map_cx t =
      SigHash.add_destructor sig_hash t;
      match t with
      | NonMaybeType -> t
      | PropertyType s ->
        SigHash.add_name sig_hash s;
        t
      | ElementType { index_type; is_indexed_access } ->
        let index_type' = self#type_ cx map_cx index_type in
        if index_type' == index_type then
          t
        else
          ElementType { index_type = index_type'; is_indexed_access }
      | Bind t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          Bind t''
      | ReadOnlyType -> t
      | SpreadType (options, tlist, acc) ->
        let () =
          match options with
          | Object.Spread.Annot { make_exact } -> SigHash.add_bool sig_hash make_exact
          | _ -> ()
        in
        let tlist' = ListUtils.ident_map (self#object_kit_spread_operand cx map_cx) tlist in
        let acc' = OptionUtils.ident_map (self#object_kit_spread_operand_slice cx map_cx) acc in
        if tlist' == tlist && acc == acc' then
          t
        else
          SpreadType (options, tlist', acc')
      | RestType (options, x) ->
        let open Object.Rest in
        let () =
          match options with
          | Sound -> SigHash.add_int sig_hash 1
          | IgnoreExactAndOwn -> SigHash.add_int sig_hash 2
          | ReactConfigMerge p ->
            SigHash.add_int sig_hash 3;
            SigHash.add sig_hash (Polarity.sigil p)
        in
        let x' = self#type_ cx map_cx x in
        if x' == x then
          t
        else
          RestType (options, x')
      | ValuesType -> t
      | CallType args ->
        let args' = ListUtils.ident_map (self#type_ cx map_cx) args in
        if args' == args then
          t
        else
          CallType args'
      | TypeMap tmap ->
        let tmap' = self#type_map cx map_cx tmap in
        if tmap' == tmap then
          t
        else
          TypeMap tmap'
      | ReactConfigType default_props ->
        let default_props' = self#type_ cx map_cx default_props in
        if default_props' == default_props then
          t
        else
          ReactConfigType default_props'
      | ReactElementPropsType
      | ReactElementConfigType
      | ReactElementRefType ->
        t

    method! dict_type cx pole dicttype =
      let dicttype' = super#dict_type cx pole dicttype in
      SigHash.add_polarity sig_hash dicttype'.dict_polarity;
      dicttype'

    method! enum cx pole e =
      let { enum_id; members; _ } = e in
      let e' = super#enum cx pole e in
      SigHash.add_aloc sig_hash (enum_id :> ALoc.t);
      Base.List.iter
        ~f:(SigHash.add sig_hash)
        (SMap.keys members |> Base.List.stable_sort ~compare:String.compare);
      e'

    method! type_ cx pole t =
      SigHash.add_reason sig_hash (reason_of_t t);
      begin
        match t with
        | DefT (_, trust, _) when Context.trust_tracking cx && is_ident trust ->
          ignore (self#trust_var cx pole (as_ident trust))
        | _ -> ()
      end;
      match t with
      | InternalT _ -> Utils_js.assert_false "internal types should not appear in signatures"
      | OpenT _ -> super#type_ cx pole t
      | DefT (_, _, InstanceT (_, _, _, { class_id; _ })) ->
        SigHash.add_aloc sig_hash (class_id :> ALoc.t);
        super#type_ cx pole t
      | OpaqueT (_, { opaque_id; _ }) ->
        SigHash.add_aloc sig_hash (opaque_id :> ALoc.t);
        super#type_ cx pole t
      | DefT (_, _, PolyT { id = poly_id; _ }) ->
        if Context.mem_nominal_poly_id cx poly_id then
          Poly.id_as_int poly_id
          |> Base.Option.iter ~f:(fun poly_id ->
                 let id =
                   match IMap.find_opt poly_id stable_poly_ids with
                   | None ->
                     let id = self#fresh_stable_id in
                     stable_poly_ids <- IMap.add poly_id id stable_poly_ids;
                     id
                   | Some id -> id
                 in
                 SigHash.add_int sig_hash id)
        else
          Poly.id_as_int poly_id |> Base.Option.iter ~f:(SigHash.add_int sig_hash);
        super#type_ cx pole t
      | UnionT (_, rep) ->
        if not (UnionRep.is_optimized_finally rep) then
          UnionRep.optimize
            rep
            ~reasonless_eq:TypeUtil.reasonless_eq
            ~flatten:(Type_mapper.union_flatten cx)
            ~find_resolved:(Context.find_resolved cx)
            ~find_props:(Context.find_props cx);
        let t' = super#type_ cx pole t in
        SigHash.add_type sig_hash t';
        t'
      | _ ->
        let t' = super#type_ cx pole t in
        SigHash.add_type sig_hash t';
        t'

    method! use_type cx pole use =
      SigHash.add_reason sig_hash (reason_of_use_t use);
      match use with
      | UseT (u, t) ->
        let t' = self#type_ cx Polarity.Neutral t in
        if t' == t then
          use
        else
          UseT (u, t')
      | _ ->
        SigHash.add_use sig_hash use;
        super#use_type cx pole use

    method! choice_use_tool =
      (* Any choice kit constraints should be fully
       * discharged by this point. This preserves a key invariant, that type
       * graphs are local to a single merge job. In other words, we will not see
       * a FullyResolveType constraint that corresponds to a tvar from another
       * context. This makes it possible to clear the type graph before storing
       * in the heap. *)
      Utils_js.assert_false "choice kit uses should not appear in signatures"

    (* We need to make sure to hash the keys in any spread intermediate types! *)
    method! object_kit_spread_operand_slice cx map_cx slice =
      NameUtils.Map.iter (fun k _ -> SigHash.add_name sig_hash k) slice.Object.Spread.prop_map;
      super#object_kit_spread_operand_slice cx map_cx slice

    method get_reduced_module_map = reduced_module_map

    method get_reduced_graph = reduced_graph

    method get_reduced_trust_graph = reduced_trust_graph

    method get_reduced_property_maps = reduced_property_maps

    method get_reduced_call_props = reduced_call_props

    method get_reduced_export_maps = reduced_export_maps

    method get_reduced_evaluated = reduced_evaluated
  end

let reduce_context cx ~no_lowers module_refs =
  let reducer = new context_optimizer ~no_lowers in
  Base.List.iter ~f:(reducer#reduce cx) module_refs;
  (reducer#sig_hash (), reducer)
