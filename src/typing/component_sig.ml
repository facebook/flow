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

class component_scope_visitor cx ~return_t =
  object (this)
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    method on_type_annot x = x

    method on_loc_annot x = x

    method! function_ fn = fn

    method! component_declaration c = c

    method visit statements = ignore @@ this#statement_list statements

    (* Override statement so that we have the loc for return *)
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
      Flow.flow cx (t, UseT (use_op, return_t))
  end

module Make
    (Statement : Statement_sig.S)
    (CT : Component_sig_types.ParamConfig.S)
    (C : Component_params_intf.Config with module Types := CT)
    (F : Component_params.S with module Config_types := CT and module Config := C)
    (T : Component_sig_types.ComponentSig.S with module Config := CT and module Param := F.Types) :
  Component_sig_intf.S
    with module Config_types := CT
     and module Config := C
     and module Param := F
     and module Types = T = struct
  module Types = T

  let toplevels cx x =
    let { T.reason = _; cparams; body; ret_annot_loc = _; return_t; _ } = x in
    let (_, body_block) = body in

    (* add param bindings *)
    let params_ast = F.eval cx cparams in

    let open Ast.Statement in
    let (statements, reconstruct_body) =
      (body_block.Block.body, (fun body -> { body_block with Block.body }))
    in

    (* statement visit pass *)
    let (statements_ast, _statements_abnormal) =
      Toplevels.toplevels Statement.statement cx statements
    in
    (* TODO(jmbrown): Error on implicit returns unless enum exhaustiveness check passes *)
    let body_ast = reconstruct_body statements_ast in
    let () = (new component_scope_visitor cx ~return_t)#visit statements_ast in

    (params_ast, body_ast)

  let component_type cx _component_loc x =
    let { T.reason; tparams; cparams; return_t; _ } = x in
    let this_type = Type.implicit_mixed_this reason in
    (* TODO(jmbrown): Better reasons for config. For now, we re-use the component
     * reason *)
    let funtype =
      {
        Type.this_t = (this_type, This_Function);
        params = [(None, F.config cx reason cparams)];
        rest_param = None;
        return_t;
        predicate = None;
        def_reason = reason;
      }
    in
    let statics_t =
      Flow_js.get_builtin_type cx reason (OrdinaryName "React$AbstractComponentStatics")
    in
    let make_trust = Context.trust_constructor cx in
    let t = DefT (reason, make_trust (), FunT (statics_t, funtype)) in
    poly_type_of_tparams (Type.Poly.generate_id ()) tparams t
end
