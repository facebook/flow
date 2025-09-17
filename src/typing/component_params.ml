(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Given the common representation for parameters, we can extract a config type for any kind of
 * AST node that introduces a component.
 *
 * Module is defined here instead of component_sig.ml because it is used in statement_sig.ml and
 * would otherwise cause a cycle.
 *)
module type S = sig
  module Config_types : Component_sig_types.ParamConfig.S

  module Config : Component_params_intf.Config with module Types := Config_types

  module Types : Component_sig_types.ParamTypes.S with module Config := Config_types

  val empty : Types.reconstruct -> Types.t

  val add_param : Config_types.param -> Types.t -> Types.t

  val add_rest : Config_types.rest -> Types.t -> Types.t

  val config_and_instance_ignored_when_ref_stored_in_props :
    Context.t ->
    in_annotation:bool ->
    config_reason:Reason.reason ->
    instance_reason:Reason.reason ->
    Types.t ->
    Type.t * Type.component_instance

  val eval : Context.t -> Types.t -> (ALoc.t * Type.t) Config_types.ast
end

exception Found of string

let tparam_finder =
  let open Type in
  object
    inherit [Subst_name.Set.t] Type_visitor.t as super

    method! type_ cx pole tparam_names =
      function
      | GenericT { name = s; _ } as t ->
        if Subst_name.Set.mem s tparam_names then
          raise (Found (Subst_name.string_of_subst_name s))
        else
          super#type_ cx pole tparam_names t
      (* We remove any tparam names from the map when entering a PolyT to avoid naming conflicts. *)
      | DefT (_, PolyT { tparams; _ }) as t ->
        let tparam_names' =
          Nel.fold_left (fun names x -> Subst_name.Set.remove x.name names) tparam_names tparams
        in
        let (_ : Subst_name.Set.t) = super#type_ cx pole tparam_names' t in
        tparam_names
      | t -> super#type_ cx pole tparam_names t
  end

module Make
    (CT : Component_sig_types.ParamConfig.S)
    (C : Component_params_intf.Config with module Types := CT)
    (T : Component_sig_types.ParamTypes.S with module Config := CT) :
  S with module Config_types := CT and module Config := C and module Types = T = struct
  module Types = T
  open T

  let empty reconstruct = { params_rev = []; rest = None; reconstruct }

  let add_param p x = { x with params_rev = p :: x.params_rev }

  let add_rest r x = { x with rest = Some r }

  let config_and_instance_ignored_when_ref_stored_in_props
      cx ~in_annotation ~config_reason ~instance_reason { params_rev; rest; reconstruct = _ } =
    let (pmap, ref_prop) =
      List.fold_left
        (fun (acc, ref_prop) p ->
          let (key_loc, key, t) = C.param_type_with_name p in
          let ref_prop =
            if key = "ref" then
              Some (key_loc, t)
            else
              ref_prop
          in
          let acc =
            match Context.react_ref_as_prop cx with
            | Options.ReactRefAsProp.StoreRefAndPropsSeparately ->
              if key = "ref" then
                acc
              else
                Type.Properties.add_field
                  (Reason.OrdinaryName key)
                  Polarity.Positive
                  ~key_loc:(Some key_loc)
                  t
                  acc
            | Options.ReactRefAsProp.StoreRefInProps ->
              Type.Properties.add_field
                (Reason.OrdinaryName key)
                Polarity.Positive
                ~key_loc:(Some key_loc)
                t
                acc
          in
          (acc, ref_prop))
        (NameUtils.Map.empty, None)
        params_rev
    in
    let rest_t =
      match rest with
      | None ->
        Obj_type.mk_with_proto cx config_reason ~obj_kind:Type.Exact (Type.ObjProtoT config_reason)
      | Some rest ->
        let t = C.rest_type rest in
        (* The rest param must be an object type. *)
        let () =
          let flags =
            Type.
              { obj_kind = Inexact; react_dro = Some (Reason.loc_of_reason config_reason, Props) }
          in
          let call = None in
          let pmap = Context.generate_property_map cx NameUtils.Map.empty in
          let rest_param_reason = TypeUtil.reason_of_t t in
          let use_op =
            Type.(Op (ComponentRestParamCompatibility { rest_param = rest_param_reason }))
          in
          (* The reason doesn't matter because in our special use_op handling we do not
           * reference the object reason *)
          let inexact_empty_obj =
            let open Type in
            mk_object_def_type ~reason:config_reason ~flags ~call pmap (ObjProtoT rest_param_reason)
          in
          Context.add_post_inference_subtyping_check cx t use_op inexact_empty_obj
        in
        t
    in
    let instance_ignored_when_ref_stored_in_props =
      match ref_prop with
      | None -> Type.ComponentInstanceOmitted instance_reason
      | Some (key_loc, ref_prop) ->
        C.read_react cx key_loc;
        let open Type in
        let () =
          if not in_annotation then
            let open Reason in
            let reason_op = mk_reason RReactRef key_loc in
            let u =
              Flow_js.get_builtin_react_typeapp
                cx
                reason_op
                Flow_intermediate_error_types.ReactModuleForReactRefSetterType
                [AnyT.error reason_op]
            in
            Context.add_post_inference_subtyping_check
              cx
              ref_prop
              (Op (DeclareComponentRef { op = reason_op }))
              u
        in
        ComponentInstanceAvailableAsRefSetterProp ref_prop
    in
    let allow_ref_in_spread =
      match Context.react_ref_as_prop cx with
      | Options.ReactRefAsProp.StoreRefAndPropsSeparately -> false
      | Options.ReactRefAsProp.StoreRefInProps -> in_annotation
    in
    let config =
      Type.(
        Flow_js.mk_possibly_evaluated_destructor
          cx
          unknown_use
          config_reason
          rest_t
          (ReactCheckComponentConfig { props = pmap; allow_ref_in_spread })
          (Eval.generate_id ())
      )
    in
    (config, instance_ignored_when_ref_stored_in_props)

  let eval cx { params_rev; rest; reconstruct } =
    let params = List.rev params_rev in
    let param_tasts_rev = List.rev_map (C.eval_param cx) params in
    let rest_tast = Base.Option.map ~f:(C.eval_rest cx) rest in
    reconstruct (List.rev param_tasts_rev) rest_tast
end
