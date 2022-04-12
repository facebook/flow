(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Name_def
open Type
open Reason
open Loc_collections
module Ast = Flow_ast

module type S = sig
  val resolve_component :
    Context.t -> (Name_def.def * reason) ALocMap.t -> Name_def_ordering.result -> unit
end

module Make (Env : Env_sig.S) (Statement : Statement_sig.S with module Env := Env) : S = struct
  module Type_annotation = Statement.Anno

  let rec resolve_binding cx loc b =
    let cache = Context.node_cache cx in
    let expression ~hint ?cond exp =
      let (((_, t), _) as exp) = Statement.expression ~hint ?cond cx exp in
      Node_cache.set_expression cache exp;
      t
    in
    match b with
    | Root (Annotation anno) ->
      let (t, anno) = Type_annotation.mk_type_available_annotation cx Subst_name.Map.empty anno in
      Node_cache.set_annotation cache anno;
      t
    | Root (Value exp) ->
      (* TODO: look up the annotation for the variable at loc and pass in *)
      expression ~hint:None exp
    | Root (Contextual _) -> Tvar.mk cx (mk_reason (RCustom "contextual variable") loc)
    | Root Catch -> AnyT.annot (mk_reason (RCustom "catch parameter") loc)
    | Root (For (kind, exp)) ->
      let reason = mk_reason (RCustom "for-in") loc (*TODO: loc should be loc of loop *) in
      let right_t = expression ~hint:None ~cond:OtherTest exp in
      begin
        match kind with
        | In ->
          Flow_js.flow cx (right_t, AssertForInRHST reason);
          StrT.at loc |> with_trust bogus_trust
        | Of { await } -> Statement.for_of_elemt cx right_t reason await
      end
    | Select (sel, b) ->
      let t = resolve_binding cx loc b in
      let selector =
        match sel with
        | Name_def.Elem n ->
          let key =
            DefT
              ( mk_reason RNumber loc,
                bogus_trust (),
                NumT (Literal (None, (float n, string_of_int n)))
              )
          in
          Type.Elem key
        | Name_def.Prop { prop; has_default } -> Type.Prop (prop, has_default)
        | Name_def.ArrRest n -> Type.ArrRest n
        | Name_def.ObjRest { used_props; after_computed = _ } ->
          (* TODO: eveyrthing after a computed prop should be optional *)
          Type.ObjRest used_props
        | Name_def.Computed exp ->
          let t = expression ~hint:None exp in
          Type.Elem t
        | Name_def.Default _exp ->
          (* TODO: change the way default works to see exp as a source *)
          Type.Default
      in
      let reason = mk_reason (RCustom "destructured var") loc in
      Tvar.mk_no_wrap_where cx reason (fun tout ->
          Flow_js.flow
            cx
            (t, DestructuringT (reason, DestructInfer, selector, tout, Reason.mk_id ()))
      )

  let resolve_inferred_function cx id_loc reason function_ =
    let cache = Context.node_cache cx in
    let ((fun_type, _) as fn) =
      (* TODO: This is intended to be the general type for the variable in the old environment, needed
         for generic escape detection. We can do generic escape differently in the future and remove
         this when we kill the old env. *)
      let general = Tvar.mk cx reason in
      Statement.mk_function cx ~hint:None ~needs_this_param:true ~general reason function_
    in
    Node_cache.set_function cache id_loc fn;
    fun_type

  let resolve_annotated_function cx reason ({ Ast.Function.body; params; _ } as function_) =
    let (({ Statement.Func_stmt_sig.fparams; _ } as func_sig), _) =
      Statement.mk_func_sig
        cx
        ~hint:None
        ~needs_this_param:true
        Subst_name.Map.empty
        reason
        function_
    in
    let this_t =
      let default =
        if Signature_utils.This_finder.found_this_in_body_or_params body params then
          let loc = aloc_of_reason reason in
          Tvar.mk cx (mk_reason RThis loc)
        else
          Type.implicit_mixed_this reason
      in
      Base.Option.value (Statement.Func_stmt_params.this fparams) ~default
    in
    Statement.Func_stmt_sig.functiontype cx this_t func_sig

  let resolve cx id_loc (def, def_reason) =
    let t =
      match def with
      | Binding (loc, b) -> resolve_binding cx loc b
      | Function { function_; fully_annotated = false } ->
        resolve_inferred_function cx id_loc def_reason function_
      | Function { function_; fully_annotated = true } ->
        resolve_annotated_function cx def_reason function_
      | _ -> Tvar.mk cx (mk_reason (RCustom "unhandled def") id_loc)
    in
    Debug_js.Verbose.print_if_verbose_lazy
      cx
      ( lazy
        [
          Printf.sprintf
            "Setting variable at %s to %s"
            (ALoc.debug_to_string id_loc)
            (Debug_js.dump_t cx t);
        ]
        );
    New_env.New_env.resolve_env_entry cx t id_loc

  let resolve_component cx graph component =
    let open Name_def_ordering in
    let resolve_element = function
      | Name_def_ordering.Normal loc
      | Resolvable loc
      | Illegal { loc; _ } ->
        resolve cx loc (ALocMap.find loc graph)
    in
    match component with
    | Singleton elt -> resolve_element elt
    | ResolvableSCC elts -> Nel.iter (fun elt -> resolve_element elt) elts
    | IllegalSCC elts -> Nel.iter (fun (elt, _, _) -> resolve_element elt) elts
end
