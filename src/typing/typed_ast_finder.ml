(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

let polarity = function
  | Some (_, { Ast.Variance.kind = Ast.Variance.Plus; comments = _ }) -> Polarity.Positive
  | Some (_, { Ast.Variance.kind = Ast.Variance.Minus; comments = _ }) -> Polarity.Negative
  | Some (_, Ast.Variance.{ kind = InOut; comments = _ }) -> Polarity.Neutral
  | Some (_, Ast.Variance.{ kind = Readonly | Out; comments = _ }) -> Polarity.Positive
  | Some (_, Ast.Variance.{ kind = In; comments = _ }) -> Polarity.Negative
  | None -> Polarity.Neutral

let mk_bound_t cx tparam = Flow_js_utils.generic_of_tparam cx ~f:(fun x -> x) tparam

class type_parameter_mapper =
  object (self)
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    method on_loc_annot (x : ALoc.t) = x

    method on_type_annot (x : ALoc.t * Type.t) = x

    (* Since the mapper wasn't originally written to pass an accumulator value
       through the calls, we're maintaining this accumulator imperatively. *)
    val mutable rev_bound_tparams : Type.typeparam list = []

    method annot_with_tparams : 'a. (tparams_rev:Type.typeparam list -> 'a) -> 'a =
      (fun f -> f ~tparams_rev:rev_bound_tparams)

    (* Imperatively adds type parameter to bound_tparams environment. *)
    method! type_param tparam =
      let res = super#type_param tparam in
      let tparam = self#make_typeparam tparam in
      rev_bound_tparams <- tparam :: rev_bound_tparams;
      res

    method private make_typeparam tparam =
      let open Ast.Type.TypeParam in
      let (_, { name = id; bound; bound_kind = _; variance; default }) = tparam in
      let (name_loc, { Ast.Identifier.name; comments = _ }) = id in
      let reason = Reason.(mk_annot_reason (RType (OrdinaryName name)) name_loc) in
      let bound =
        match bound with
        | Ast.Type.Missing (_, t)
        | Ast.Type.Available (_, ((_, t), _)) ->
          t
      in
      let default =
        match default with
        | None -> None
        | Some ((_, t), _) -> Some t
      in
      let polarity = polarity variance in
      { Type.reason; name = Subst_name.Name name; bound; polarity; default; is_this = false }

    (* Record and restore the parameter environment around nodes that might
       update it. *)
    method! type_params_opt pd f =
      let originally_bound_tparams = rev_bound_tparams in
      let res = super#type_params_opt pd f in
      rev_bound_tparams <- originally_bound_tparams;
      res

    method! conditional_type t =
      let open Ast.Type.Conditional in
      let { check_type; extends_type; true_type; false_type; comments } = t in
      let check_type' = self#type_ check_type in
      let extends_type' = self#type_ extends_type in
      let fake_tparams_opt =
        let params =
          Infer_type_hoister.hoist_infer_types extends_type
          |> Base.List.map ~f:(fun (_, { Ast.Type.Infer.tparam; _ }) -> tparam)
        in
        match params with
        | [] -> None
        | (loc, _) :: _ -> Some (loc, { Ast.Type.TypeParams.params; comments = None })
      in
      let true_type' = self#type_params_opt fake_tparams_opt (fun _ -> self#type_ true_type) in
      let false_type' = self#type_ false_type in
      let comments' = self#syntax_opt comments in
      {
        check_type = check_type';
        extends_type = extends_type';
        true_type = true_type';
        false_type = false_type';
        comments = comments';
      }

    (* Classes assume an additional "this" type parameter, which needs to be
       explicitly added to bound_tparams *)
    method! class_ cls =
      let open Reason in
      let { Ast.Class.body = (body_loc, _); id; _ } = cls in
      let bound =
        match id with
        | Some ((_, t), _) -> t
        | None ->
          let reason = mk_reason (RCustom "<<anonymous class>>") body_loc in
          Type.DefT (reason, Type.MixedT Type.Mixed_everything)
      in
      let this_tparam =
        {
          Type.name = Subst_name.Name "this";
          reason = replace_desc_reason RThisType (TypeUtil.reason_of_t bound);
          bound;
          polarity = Polarity.Positive;
          default = None;
          is_this = true;
        }
      in
      let originally_bound_tparams = rev_bound_tparams in
      rev_bound_tparams <- this_tparam :: rev_bound_tparams;
      let cls = super#class_ cls in
      rev_bound_tparams <- originally_bound_tparams;
      cls

    method! declare_class decl =
      let open Reason in
      let { Ast.Statement.DeclareClass.id; _ } = decl in
      let ((_, bound), _) = id in
      let this_tparam =
        {
          Type.name = Subst_name.Name "this";
          reason = replace_desc_reason RThisType (TypeUtil.reason_of_t bound);
          bound;
          polarity = Polarity.Positive;
          default = None;
          is_this = true;
        }
      in
      let originally_bound_tparams = rev_bound_tparams in
      rev_bound_tparams <- this_tparam :: rev_bound_tparams;
      let decl = super#declare_class decl in
      rev_bound_tparams <- originally_bound_tparams;
      decl
  end

(* Find exact location match *)
module ExactMatchQuery = struct
  exception Found of Type.TypeScheme.t

  let found ~tparams_rev t = raise (Found { Type.TypeScheme.tparams_rev; type_ = t })

  class exact_match_searcher cx (target_loc : ALoc.t) =
    object (self)
      inherit type_parameter_mapper as super

      method! type_param ((_, { Ast.Type.TypeParam.name = (loc, _); _ }) as tparam) =
        if target_loc = loc then (
          let tparam = self#make_typeparam tparam in
          rev_bound_tparams <- tparam :: rev_bound_tparams;
          self#annot_with_tparams (found (mk_bound_t cx tparam))
        ) else
          super#type_param tparam

      method! on_type_annot annot =
        let (loc, t) = annot in
        if target_loc = loc then
          self#annot_with_tparams (found t)
        else
          super#on_type_annot annot
    end

  let find cx typed_ast aloc =
    let searcher = new exact_match_searcher cx aloc in
    try
      ignore (searcher#program typed_ast);
      None
    with
    | Found scheme -> Some scheme
end

let find_exact_match_annotation = ExactMatchQuery.find
