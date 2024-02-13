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

  val config_and_instance :
    Context.t ->
    config_reason:Reason.reason ->
    instance_reason:Reason.reason ->
    tparams:Type.typeparams ->
    Types.t ->
    Type.t * Type.t

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

  let config_and_instance
      cx ~config_reason ~instance_reason ~tparams { params_rev; rest; reconstruct = _ } =
    let (pmap, instance) =
      List.fold_left
        (fun (acc, instance) p ->
          let key_and_t = C.param_type_with_name p in
          match key_and_t with
          | (key_loc, "ref", t) ->
            let ref =
              match tparams with
              | None -> Some (key_loc, t)
              | Some (_, ({ Type.name; _ }, tps)) ->
                let names =
                  Base.List.fold
                    ~f:(fun acc { Type.name; _ } -> Subst_name.Set.add name acc)
                    ~init:(Subst_name.Set.singleton name)
                    tps
                in
                (try
                   let (_ : Subst_name.Set.t) = tparam_finder#type_ cx Polarity.Neutral names t in
                   Some (key_loc, t)
                 with
                | Found name -> begin
                  Flow_js_utils.add_output cx Error_message.(EInvalidRef (key_loc, name));
                  None
                end)
            in
            (acc, ref)
          | (key_loc, key, t) ->
            ( Type.Properties.add_field
                (Reason.OrdinaryName key)
                Polarity.Positive
                ~key_loc:(Some key_loc)
                t
                acc,
              instance
            ))
        (NameUtils.Map.empty, None)
        params_rev
    in
    let rest_t =
      match rest with
      | None -> Obj_type.mk_exact_empty cx config_reason
      | Some rest ->
        let t = C.rest_type rest in
        (* The rest param must be an object type. *)
        let () =
          let inexact_empty_obj =
            let flags =
              Type.
                {
                  obj_kind = Inexact;
                  frozen = false;
                  react_dro = Some (Reason.loc_of_reason config_reason, Props);
                }
            in
            let call = None in
            let pmap = Context.generate_property_map cx NameUtils.Map.empty in
            let rest_param_reason = TypeUtil.reason_of_t t in
            let use_op =
              Type.(Op (ComponentRestParamCompatibility { rest_param = rest_param_reason }))
            in
            (* The reason doesn't matter because in our special use_op handling we do not
             * reference the object reason *)
            Type.(
              UseT
                ( use_op,
                  mk_object_def_type
                    ~reason:config_reason
                    ~flags
                    ~call
                    pmap
                    (ObjProtoT rest_param_reason)
                )
            )
          in
          Flow_js.flow cx (t, inexact_empty_obj)
        in

        t
    in
    let instance =
      match instance with
      | None -> Type.MixedT.make instance_reason
      | Some (key_loc, instance) ->
        C.read_react cx key_loc;
        let () =
          let open Type in
          let open Reason in
          let reason_op = mk_reason RReactRef key_loc in
          let u =
            Flow_js.get_builtin_typeapp cx reason_op "React$RefSetter" [AnyT.error reason_op]
          in
          Flow_js.flow cx (instance, UseT (Op (DeclareComponentRef { op = reason_op }), u))
        in
        Type.(
          Flow_js.mk_possibly_evaluated_destructor
            cx
            unknown_use
            instance_reason
            instance
            ReactCheckComponentRef
            (Eval.generate_id ())
        )
    in
    let config =
      Type.(
        Flow_js.mk_possibly_evaluated_destructor
          cx
          unknown_use
          config_reason
          rest_t
          (ReactCheckComponentConfig pmap)
          (Eval.generate_id ())
      )
    in
    (config, instance)

  let eval cx { params_rev; rest; reconstruct } =
    let params = List.rev params_rev in
    let param_tasts_rev = List.rev_map (C.eval_param cx) params in
    let rest_tast = Base.Option.map ~f:(C.eval_rest cx) rest in
    reconstruct (List.rev param_tasts_rev) rest_tast
end
