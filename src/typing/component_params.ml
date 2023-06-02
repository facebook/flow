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

  module Config : Component_sig_types.Config with module Types := Config_types

  module Types : Component_sig_types.ParamTypes.S with module Config := Config_types

  val empty : Types.reconstruct -> Types.t

  val add_param : Config_types.param -> Types.t -> Types.t

  val add_rest : Config_types.rest -> Types.t -> Types.t

  val config : Context.t -> Reason.reason -> Types.t -> Type.t

  val eval : Context.t -> Types.t -> (ALoc.t * Type.t) Config_types.ast
end

module Make
    (CT : Component_sig_types.ParamConfig.S)
    (C : Component_sig_types.Config with module Types := CT)
    (T : Component_sig_types.ParamTypes.S with module Config := CT) :
  S with module Config_types := CT and module Config := C and module Types = T = struct
  module Types = T
  open T

  let empty reconstruct = { params_rev = []; rest = None; reconstruct }

  let add_param p x = { x with params_rev = p :: x.params_rev }

  let add_rest r x = { x with rest = Some r }

  let config cx config_reason { params_rev; rest; reconstruct = _ } =
    let pmap =
      List.fold_left
        (fun acc p ->
          let key_and_t_opt = C.param_type_with_name p in
          match key_and_t_opt with
          | None ->
            (* Unnamed props are a parser error, so we do not handle them here *)
            acc
          | Some (key_loc, key, t) ->
            Type.Properties.add_field
              (Reason.OrdinaryName key)
              Polarity.Positive
              (Some key_loc)
              t
              acc)
        NameUtils.Map.empty
        params_rev
    in
    let rest_t =
      match rest with
      | None -> Obj_type.mk_exact_empty cx config_reason
      | Some rest -> C.rest_type rest
    in
    Type.(
      EvalT
        ( rest_t,
          TypeDestructorT (unknown_use, config_reason, ReactCheckComponentConfig pmap),
          Eval.generate_id ()
        )
    )

  let eval cx { params_rev; rest; reconstruct } =
    let params = List.rev params_rev in
    let param_tasts_rev = List.rev_map (C.eval_param cx) params in
    let rest_tast = Base.Option.map ~f:(C.eval_rest cx) rest in
    reconstruct (List.rev param_tasts_rev) rest_tast
end
