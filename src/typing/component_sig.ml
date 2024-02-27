(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module Flow = Flow_js
open Reason
open Type
open TypeUtil

module Component_declaration_body
    (Statement : Statement_sig.S)
    (Config : Component_sig_types.BodyConfig.S) =
struct
  module Config = Config

  class component_scope_visitor cx ~renders_t exhaust =
    object (this)
      inherit
        [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

      method on_type_annot x = x

      method on_loc_annot x = x

      method! function_ fn = fn

      method! component_declaration c = c

      method visit statements = ignore @@ this#statement_list statements

      method! switch ({ Ast.Statement.Switch.exhaustive_out = (loc, t); _ } as switch) =
        Base.Option.iter exhaust ~f:(fun (exhaustive_t, exhaust_locs, _) ->
            if Base.List.mem ~equal:ALoc.equal exhaust_locs loc then Flow.flow_t cx (t, exhaustive_t)
        );
        super#switch switch

      (* Override statement so that we have the loc for renders *)
      method! statement (loc, stmt) =
        begin
          match stmt with
          | Ast.Statement.Return return -> this#custom_return return
          | _ -> ()
        end;
        super#statement (loc, stmt)

      method custom_return { Ast.Statement.Return.return_out = (_, t); argument; _ } =
        let use_op =
          Op
            (FunReturnStatement
               {
                 value =
                   Base.Option.value_map argument ~default:(reason_of_t t) ~f:(fun expr ->
                       mk_expression_reason (Typed_ast_utils.untyped_ast_mapper#expression expr)
                   );
               }
            )
        in
        Flow.flow cx (t, UseT (use_op, renders_t))
    end

  let eval cx reason_cmp renders_t (body_loc, body_block) =
    let open Ast.Statement in
    let (statements, reconstruct_body) =
      (body_block.Block.body, (fun body -> { body_block with Block.body }))
    in

    (* statement visit pass *)
    let statements_ast = Statement.statement_list cx statements in

    let exhaust =
      let use_op = unknown_use in
      let (exhaustive, undeclared) = Context.exhaustive_check cx body_loc in
      if undeclared then begin
        Flow_js_utils.add_output cx Error_message.(EComponentMissingReturn reason_cmp);
        None
      end else
        Some
          ( Tvar.mk cx (replace_desc_reason (RCustom "maybe_exhaustively_checked") reason_cmp),
            exhaustive,
            ImplicitVoidReturnT
              { use_op; reason = reason_of_t renders_t; action = NoImplicitReturns reason_cmp }
          )
    in

    let body_ast = reconstruct_body statements_ast in
    let () = (new component_scope_visitor cx ~renders_t exhaust)#visit statements_ast in

    Base.Option.iter exhaust ~f:(fun (maybe_exhaustively_checked, _, implicit_return) ->
        Flow.flow cx (maybe_exhaustively_checked, implicit_return)
    );
    (body_loc, body_ast)
end

module Make
    (CT : Component_sig_types.ParamConfig.S)
    (C : Component_params_intf.Config with module Types := CT)
    (F : Component_params.S with module Config_types := CT and module Config := C)
    (BC : Component_sig_types.BodyConfig.S)
    (B : Component_sig_intf.ComponentBody with module Config := BC)
    (T : Component_sig_types.ComponentSig.S
           with module Config := CT
            and module Param := F.Types
            and module Body := BC) :
  Component_sig_intf.S
    with module BodyConfig := BC
     and module Config_types = CT
     and module Config = C
     and module Param = F
     and module Types = T
     and module ComponentBody = B = struct
  module Types = T
  module Config_types = CT
  module Param = F
  module Config = C
  module ComponentBody = B

  let toplevels cx x =
    let { T.reason = reason_cmp; cparams; body; ret_annot_loc = _; renders_t; _ } = x in
    (* add param bindings *)
    let params_ast = F.eval cx cparams in
    let body_ast = B.eval cx reason_cmp renders_t body in
    (params_ast, body_ast)

  let component_type cx _component_loc x =
    let { T.reason; tparams; cparams; renders_t; id_opt; _ } = x in
    let config_reason = update_desc_reason (fun desc -> RPropsOfComponent desc) reason in
    let instance_reason = update_desc_reason (fun desc -> RInstanceOfComponent desc) reason in
    let (config, instance) =
      F.config_and_instance cx ~config_reason ~instance_reason ~tparams cparams
    in
    let () =
      (* render types must be a subtype of React.Node
       * TODO(jmbrown): This check can be skipped if we track whether or not the renders type was
       * explicitly annotated *)
      let renders_reason = TypeUtil.reason_of_t renders_t in
      let t = Flow.get_builtin_type cx (TypeUtil.reason_of_t renders_t) "React$Node" in
      let use_op = Op (RenderTypeInstantiation { render_type = renders_reason }) in
      Context.add_post_inference_subtyping_check cx renders_t use_op t
    in
    let component_kind =
      match id_opt with
      | None -> Structural
      | Some (id_loc, name) ->
        let opaque_id = Context.make_aloc_id cx id_loc in
        Nominal (opaque_id, name)
    in
    let t =
      DefT
        (reason, ReactAbstractComponentT { config; instance; renders = renders_t; component_kind })
    in
    poly_type_of_tparams (Type.Poly.generate_id ()) tparams t
end
