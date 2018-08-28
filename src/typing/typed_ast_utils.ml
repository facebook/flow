(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

class type_parameter_mapper = object(_)
  inherit [
    Loc.t, Loc.t * Type.t,
    Loc.t, Loc.t * Type.t
  ] Flow_polymorphic_ast_mapper.mapper as super

  method on_loc_annot (x: Loc.t) = x
  method on_type_annot (x: Loc.t * Type.t) = x

  (* Since the mapper wasn't originally written to pass an accumulator value
     through the calls, we're maintaining this accumulator imperatively. *)
  val mutable bound_tparams : Type.typeparam list = []

  method annot_with_tparams : 'a . (Type.typeparam list -> 'a) -> 'a =
    fun f -> f bound_tparams

  (* Imperatively adds type parameter to bound_tparams environment. *)
  method! type_parameter_declaration_type_param tparam =
    let res = super#type_parameter_declaration_type_param tparam in
    (* Recover the Type.typeparams corresponding to AST type parameters *)
    let tparam = Ast.Type.ParameterDeclaration.(
      let _, { TypeParam.name = (_, t), name; bound; variance; default; } = tparam in
      let reason = Type.reason_of_t t in
      let bound = match bound with
      | None -> Type.MixedT.make reason
      | Some (_, ((_, t), _)) -> t
      in
      let polarity = Type_annotation.polarity variance in
      let default = Option.map default ~f:(fun ((_, t), _) -> t)
      in
      { Type.reason; name; bound; polarity; default; }
    ) in
    bound_tparams <- tparam :: bound_tparams;
    res

  (* Record and restore the parameter environment around nodes that might
     update it. *)
  method! type_parameter_declaration_opt pd f =
    let originally_bound_tparams = bound_tparams in
    let res = super#type_parameter_declaration_opt pd f in
    bound_tparams <- originally_bound_tparams;
    res

  (* Classes assume an additional "this" type parameter, which needs to be
     explicitly added to bound_tparams *)
  method! class_ cls =
    let this_tparam = Ast.Class.(
      let { body = ((body_loc, self_t), _); id; _ } = cls in
      let name = Option.value_map ~f:snd id ~default:"<<anonymous class>>" in
      let name_loc = Option.value_map ~f:(fun ((loc, _), _) -> loc) id ~default:body_loc in
      { Type.
        name = "this";
        reason = Reason.mk_reason (Reason.RType name) name_loc;
        bound = self_t;
        polarity = Type.Positive;
        default = None;
      }
    ) in
    let originally_bound_tparams = bound_tparams in
    bound_tparams <- this_tparam :: bound_tparams;
    let cls = super#class_ cls in
    bound_tparams <- originally_bound_tparams;
    cls

end
