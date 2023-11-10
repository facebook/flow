(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Reason

let check_ref_use cx rrid var_reason kind t =
  let rec recur_id seen t =
    let recur = recur_id seen in
    let open Type in
    match t with
    | OpaqueT (_, { opaque_id; _ })
      when Base.Option.value_map ~default:false ~f:(( = ) opaque_id) rrid ->
      Flow_js_utils.add_output cx (Error_message.EReactRefInRender { usage = var_reason; kind })
    | OpaqueT (_, { underlying_t; super_t; _ }) ->
      Base.Option.iter ~f:recur underlying_t;
      Base.Option.iter ~f:recur super_t
    | OpenT (_, id) when ISet.mem id seen -> ()
    | OpenT (_, id) ->
      Flow_js_utils.possible_types cx id |> Base.List.iter ~f:(recur_id (ISet.add id seen))
    | UnionT (_, rep) ->
      let (_ : UnionRep.t) =
        UnionRep.ident_map
          (fun t ->
            recur t;
            t)
          rep
      in
      ()
    | IntersectionT (_, rep) ->
      let (_ : InterRep.t) =
        InterRep.ident_map
          (fun t ->
            recur t;
            t)
          rep
      in
      ()
    | MaybeT (_, t)
    | OptionalT { type_ = t; _ }
    | ExactT (_, t)
    | AnnotT (_, t, _)
    | TypeAppT { type_ = t; _ }
    | GenericT { bound = t; _ }
    | DefT (_, PolyT { t_out = t; _ })
    | DefT (_, TypeT (_, t)) ->
      recur t
    | _ -> ()
  in
  recur_id ISet.empty t

let rec whole_ast_visitor cx rrid =
  object (this)
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    method on_loc_annot l = l

    method on_type_annot l = l

    method! component_declaration = (component_ast_visitor cx rrid)#component_declaration

    method! function_declaration fn =
      let {
        Ast.Function.id;
        params = (_, { Ast.Function.Params.params = params_list; rest; _ }) as params;
        body;
        async;
        generator;
        predicate;
        return;
        tparams;
        sig_loc;
        comments;
      } =
        fn
      in
      if
        Context.react_rules_always cx
        && Base.Option.value_map
             ~f:(fun (_, { Ast.Identifier.name; _ }) ->
               String.length name > 0
               &&
               let fst = String.sub name 0 1 in
               fst = String.uppercase_ascii fst && fst <> "_")
             ~default:false
             id
        && List.length params_list = 1
        && Base.Option.is_none rest
      then
        let ident' = Base.Option.map ~f:this#function_identifier id in
        this#type_params_opt tparams (fun tparams' ->
            let params' = (component_ast_visitor cx rrid)#function_params params in
            let return' = this#function_return_annotation return in
            let body' = (component_ast_visitor cx rrid)#function_component_body body in
            let predicate' =
              Base.Option.map ~f:(component_ast_visitor cx rrid)#predicate predicate
            in
            let sig_loc' = this#on_loc_annot sig_loc in
            let comments' = this#syntax_opt comments in
            {
              Ast.Function.id = ident';
              params = params';
              return = return';
              body = body';
              async;
              generator;
              predicate = predicate';
              tparams = tparams';
              sig_loc = sig_loc';
              comments = comments';
            }
        )
      else
        super#function_declaration fn
  end

and component_ast_visitor cx rrid =
  object (this)
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    method on_loc_annot l = l

    method on_type_annot l = l

    method target_expression (((loc, ty), exp) as expr) err_kind =
      let reason =
        match exp with
        | Ast.Expression.Identifier ((loc, _), { Ast.Identifier.name; _ }) ->
          mk_reason (RIdentifier (OrdinaryName name)) loc
        | _ -> mk_reason (RCustom "expression") loc
      in
      if Context.react_rule_enabled cx Options.ValidateRefAccessDuringRender then
        check_ref_use cx rrid reason err_kind ty;
      this#expression expr

    method! arg_list (annot, args) =
      let open Ast.Expression.ArgList in
      let { arguments; _ } = args in
      let (_ : _ list) =
        Base.List.map
          ~f:(function
            | Ast.Expression.Expression exp ->
              Ast.Expression.Expression (this#target_expression exp Error_message.Argument)
            | Ast.Expression.Spread spread -> Ast.Expression.Spread (this#spread_element spread))
          arguments
      in
      (annot, args)

    method! member expr =
      let { Ast.Expression.Member._object; property; comments = _ } = expr in
      let (_ : (_, _) Ast.Expression.Member.property) = this#member_property property in
      let (_ : (_, _) Ast.Expression.t) =
        match property with
        | Ast.Expression.Member.PropertyIdentifier (_, { Ast.Identifier.name = "current"; _ }) ->
          this#target_expression _object Error_message.Access
        | _ -> this#expression _object
      in
      expr

    method function_component_body = super#function_body_any

    method! function_body_any = (whole_ast_visitor cx rrid)#function_body_any

    method! class_body = (whole_ast_visitor cx rrid)#class_body
  end

let check_react_rules cx ast =
  let rrid =
    let open Type in
    let get_t cx =
      let no_lowers _cx r = Type.Unsoundness.merged_any r in
      function
      | OpenT (r, id) -> Flow_js_utils.merge_tvar ~no_lowers cx r id
      | t -> t
    in

    let builtins = Context.builtins cx in
    let lhs = Builtins.get_builtin_opt builtins (OrdinaryName "React$RefObject") in
    match Base.Option.map ~f:(get_t cx) lhs with
    | Some (DefT (_, PolyT { t_out = DefT (_, TypeT (_, OpaqueT (_, { opaque_id; _ }))); _ })) ->
      Some opaque_id
    | _ -> None
  in
  let _ = (whole_ast_visitor cx rrid)#program ast in
  ()
