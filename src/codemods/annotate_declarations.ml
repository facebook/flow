(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module LMap = Loc_collections.LocMap
module LSet = Loc_collections.LocSet
open Insert_type_utils
open Loc_collections

module ErrorStats = struct
  type t = {
    num_error_vars: int;
    num_renamable_vars: int;
  }

  let empty = { num_error_vars = 0; num_renamable_vars = 0 }

  let combine c1 c2 =
    {
      num_error_vars = c1.num_error_vars + c2.num_error_vars;
      num_renamable_vars = c1.num_renamable_vars + c2.num_renamable_vars;
    }

  let serialize s =
    let open Utils_js in
    [spf "total_errors: %d" s.num_error_vars; spf "renamable_vars: %d" s.num_renamable_vars]

  let report s =
    [
      string_of_row ~indent:2 "Number of vars with write errors" s.num_error_vars;
      string_of_row ~indent:2 "Number of possibly renamable vars" s.num_renamable_vars;
    ]
end

module Codemod_declaration_annotator = Codemod_annotator.Make (ErrorStats)
module Acc = Insert_type_utils.Acc (ErrorStats)

let mapper ~preserve_literals ~max_type_size ~default_any (cctx : Codemod_context.Typed.t) =
  let { Codemod_context.Typed.file_sig; docblock; metadata; options; _ } = cctx in
  let imports_react = Insert_type_imports.ImportsHelper.imports_react file_sig in
  let metadata = Context.docblock_overrides docblock metadata in
  let { Context.strict; strict_local; _ } = metadata in
  let lint_severities =
    if strict || strict_local then
      StrictModeSettings.fold
        (fun lint_kind lint_severities ->
          LintSettings.set_value lint_kind (Severity.Err, None) lint_severities)
        (Options.strict_mode options)
        (Options.lint_severities options)
    else
      Options.lint_severities options
  in
  let suppress_types = Options.suppress_types options in
  let exact_by_default = Options.exact_by_default options in
  let flowfixme_ast = Builtins.flowfixme_ast ~lint_severities ~suppress_types ~exact_by_default in
  let cx = Codemod_context.Typed.context cctx in
  let errors = Context.errors cx in
  let loc_error_set =
    Flow_error.ErrorSet.fold
      (fun error ->
        LSet.fold
          (fun loc -> LSet.add loc)
          (Codemod_constrained_write_utils.declaration_locs_of_constrained_write_error cx error))
      errors
      LSet.empty
  in
  let extractable_assignments loc =
    (* See if we should run the rename-redefinitions codemod by seeing if any write to this variable is extractable *)
    let is_extractable =
      Codemod_constrained_write_utils.is_extractable_assignment cx loc_error_set
    in
    let { Loc_env.var_info = { Env_api.scopes; ssa_values; _ }; _ } = Context.environment cx in
    let uses = Scope_api.With_ALoc.uses_of_use scopes loc in
    ALocSet.fold
      (fun use acc ->
        match ALocMap.find_opt use ssa_values with
        | None (* is a write *) when is_extractable use -> ALocSet.add use acc
        | _ -> acc)
      uses
      ALocSet.empty
  in
  object (this)
    inherit
      Codemod_declaration_annotator.mapper
        ~max_type_size
        ~exact_by_default
        ~lint_severities
        ~suppress_types
        ~imports_react
        ~preserve_literals
        ~default_any
        cctx as super

    val mutable renamable = ALocSet.empty

    method private post_run () =
      this#add_unannotated_loc_warnings
        (LSet.fold (fun loc -> LMap.add loc ()) loc_error_set LMap.empty);
      ErrorStats.
        {
          num_error_vars = LSet.cardinal loc_error_set;
          num_renamable_vars = ALocSet.cardinal renamable;
        }

    method! variable_declarator_pattern ~kind ((ploc, patt) : ('loc, 'loc) Ast.Pattern.t) =
      let get_annot ty annot =
        let f loc _annot ty' = this#annotate_node loc ty' (fun a -> Ast.Type.Available a) in
        let error _ = Ast.Type.Available (Loc.none, flowfixme_ast) in
        this#opt_annotate ~f ~error ~expr:None ploc ty annot
      in

      if LSet.mem ploc loc_error_set then (
        let ty_result = Codemod_annotator.get_ty cctx ~preserve_literals ~max_type_size ploc in
        match patt with
        | Ast.Pattern.Object Ast.Pattern.Object.{ annot; properties; comments } ->
          let annot' = get_annot ty_result annot in
          (ploc, Ast.Pattern.Object Ast.Pattern.Object.{ annot = annot'; properties; comments })
        | Ast.Pattern.Array Ast.Pattern.Array.{ annot; elements; comments } ->
          let annot' = get_annot ty_result annot in
          (ploc, Ast.Pattern.Array Ast.Pattern.Array.{ annot = annot'; elements; comments })
        | Ast.Pattern.Identifier Ast.Pattern.Identifier.{ annot; name; optional } ->
          let annot' = get_annot ty_result annot in
          renamable <- ALocSet.union renamable (extractable_assignments (ALoc.of_loc ploc));

          (ploc, Ast.Pattern.Identifier Ast.Pattern.Identifier.{ annot = annot'; name; optional })
        | Ast.Pattern.Expression _ ->
          (* No such thing as a pattern expression *)
          this#update_acc (fun acc -> Acc.error acc ploc Error.Unsupported_error_kind);
          codemod_error_locs <- LSet.add ploc codemod_error_locs;
          (ploc, patt)
      ) else
        super#variable_declarator_pattern ~kind (ploc, patt)

    method! program prog =
      if LSet.is_empty loc_error_set then
        prog
      else
        super#program prog
  end
