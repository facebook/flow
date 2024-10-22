(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module ALocFuzzyMap = Loc_collections.ALocFuzzyMap
module RendersKit = Renders_kit.Make (Flow_js.FlowJs)

exception
  Found of {
    missing_renders_loc: Loc.t;
    body_loc: Loc.t;
  }

class mapper target =
  object (_this)
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    method! component_declaration loc component =
      let open Ast.Statement.ComponentDeclaration in
      let { id = (id_loc, _); body = (body_loc, _); renders; _ } = component in
      begin
        match renders with
        | Ast.Type.AvailableRenders _ -> ()
        | Ast.Type.MissingRenders missing_renders_loc ->
          if Loc.contains id_loc target then raise (Found { missing_renders_loc; body_loc })
      end;
      super#component_declaration loc component
  end

let insert_render_type_at_loc
    ?remote_converter
    ~cx
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_module_info
    ~get_type_sig
    ~file_sig
    ~typed_ast
    ast
    id_loc =
  let open Insert_type in
  try
    let m = new mapper id_loc in
    ignore @@ m#program ast;
    None
  with
  | Found { missing_renders_loc; body_loc } ->
    (match ALocFuzzyMap.find_opt (ALoc.of_loc body_loc) (Context.inferred_component_return cx) with
    | None -> None
    | Some ts ->
      let reason =
        Reason.(mk_reason (RRenderType (RType (OrdinaryName "React$Node"))))
          (ALoc.of_loc missing_renders_loc)
      in
      let t = TypeUtil.union_of_ts reason (Nel.to_list ts) in
      (match RendersKit.try_synthesize_render_type cx ~drop_renders_any:true t with
      | None -> None
      | Some (renders_variant, ts) ->
        let t =
          let open Type in
          DefT
            ( reason,
              RendersT
                (StructuralRenders
                   { renders_variant; renders_structural_type = TypeUtil.union_of_ts reason ts }
                )
            )
        in
        let ast' =
          insert_type_t
            ~cx
            ~loc_of_aloc
            ~get_ast_from_shared_mem
            ~get_haste_module_info
            ~get_type_sig
            ~file_sig
            ~typed_ast
            ?remote_converter
            ~omit_targ_defaults:false
            ~strict:false
            ~ambiguity_strategy:Autofix_options.Generalize
            ast
            missing_renders_loc
            t
        in
        Some ast'))
