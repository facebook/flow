(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module LSet = Loc_collections.LocSet
module ALSet = Loc_collections.ALocSet
module LMap = Loc_collections.LocMap
module Hardcoded_Ty_Fixes = Codemod_hardcoded_ty_fixes
open Insert_type_utils

(*
 * Codemod that annotates parts of a file that are visible from the exports
 * as required by Flow types-first mode.
 *
 * The main steps of this transformation pass are the following:
 *
 * 1. Use the typed codemod context, and in particular the file sig to get signature
 *    verification errors and the typed AST to query for types using the ty_normalizer.
 *
 * 2. For each supported sig. verification error, we use the type normalizer to query
 *    for the type at the location of the error, and store the results of this in
 *    a loc map.
 *
 * 3. The mapper visits the AST and for each visited location in the map from (2),
 *    decides whether to replace the AST with an annotated node. If any of the newly
 *    added types require imported symbols, then we record them to include the
 *    necessary import statement later.
 *
 * 4. Include all necessary imported statements before the main transformed body
 *    of the source code.
 *
 * We also perform a number sanity checks, to ensure that any entries from (2) were
 * explicitly handled in (3), to avoid the case where the mapper did not visit a
 * given node.
 *)

module Let_syntax = struct
  let return = return

  let bind x ~f = x >>= f

  let map x ~f = x >>| f
end

module SignatureVerification = struct
  let supported_error_kind cctx norm_opts ~max_type_size acc loc =
    let add_ty ty =
      (* NOTE simplify before validating to avoid flagging spurious empty's,
       * eg. empty's that will be simplified away as parts of unions.
       * Do not simplify empties. Ignoring some of the attendant upper bounds
       * might lead to unsound types.
       *)
      let ty = Ty_utils.simplify_type ~merge_kinds:false ty in
      match Validator.validate_type ~size_limit:max_type_size ty with
      | (ty, []) -> Ok ty
      | (ty, errs) ->
        let errs = List.map (fun e -> Error.Validation_error e) errs in
        Error (errs, ty)
    in
    let type_entry =
      match Codemod_context.Typed.ty_at_loc norm_opts cctx loc with
      | Ok (Ty.Type ty) -> add_ty ty
      | Ok (Ty.Decl (Ty.ClassDecl (s, _))) -> add_ty (Ty.TypeOf (Ty.TSymbol s))
      | Ok _ ->
        let ty = Ty.explicit_any in
        let errors = [Error.Missing_annotation_or_normalizer_error] in
        Error (errors, ty)
      | Error _ ->
        let ty = Ty.explicit_any in
        let errors = [Error.Missing_annotation_or_normalizer_error] in
        Error (errors, ty)
    in
    LMap.add loc type_entry acc

  let unsupported_error_kind ~default_any acc loc =
    if default_any then
      let ty_entry = Error ([Error.Unsupported_error_kind], Ty.explicit_any) in
      LMap.add loc ty_entry acc
    else
      acc

  let collect_annotations cctx ~preserve_literals ~default_any ~max_type_size ast =
    let preserve_inferred_literal_types =
      Hardcoded_Ty_Fixes.PreserveLiterals.(
        match preserve_literals with
        | Always
        | Auto ->
          true
        | Never -> false)
    in
    let norm_opts =
      {
        Ty_normalizer_env.fall_through_merged = false;
        expand_internal_types = false;
        expand_type_aliases = false;
        flag_shadowed_type_params = false;
        preserve_inferred_literal_types;
        evaluate_type_destructors = false;
        optimize_types = false;
        omit_targ_defaults = true;
        merge_bot_and_any_kinds = false;
        verbose_normalizer = false;
        max_depth = None;
      }
    in
    let { Codemod_context.Typed.options; docblock; _ } = cctx in
    let module_ref_prefix = Options.haste_module_ref_prefix options in
    match File_sig.With_Loc.program_with_exports_info ~ast ~module_ref_prefix with
    | Error _ -> (0, LMap.empty)
    | Ok (exports_info, _) ->
      let signature = Signature_builder.program ast ~exports_info in
      let (sig_errors, _, _) =
        let prevent_munge =
          let should_munge = Options.should_munge_underscores options in
          Docblock.preventMunge docblock || not should_munge
        in
        let facebook_fbt = Options.facebook_fbt options in
        let ignore_static_propTypes = true in
        let facebook_keyMirror = true in
        Signature_builder.Signature.verify
          ~prevent_munge
          ~facebook_fbt
          ~ignore_static_propTypes
          ~facebook_keyMirror
          signature
      in
      Signature_builder_deps.With_Loc.PrintableErrorSet.fold
        (fun err (tot_errors, acc) ->
          let open Signature_error in
          match err with
          | ExpectedAnnotation (loc, _)
          | UnexpectedExpression (loc, _)
          | UnexpectedObjectKey (loc, _)
          | EmptyArray loc
          | EmptyObject loc
          | UnexpectedArraySpread (loc, _) ->
            (tot_errors + 1, supported_error_kind cctx norm_opts ~max_type_size acc loc)
          | ExpectedSort (_, _, loc)
          | SketchyToplevelDef loc
          | UnexpectedArrayHole loc
          | UnsupportedPredicateExpression loc
          | TODO (_, loc) ->
            (tot_errors + 1, unsupported_error_kind ~default_any acc loc))
        sig_errors
        (0, LMap.empty)
end

module SignatureVerificationErrorStats = struct
  type t = {
    number_of_sig_ver_errors: int;
    number_of_annotations_required: int;
    number_of_annotations_skipped: int;
  }

  let empty =
    {
      number_of_sig_ver_errors = 0;
      number_of_annotations_required = 0;
      number_of_annotations_skipped = 0;
    }

  let combine c1 c2 =
    {
      number_of_sig_ver_errors = c1.number_of_sig_ver_errors + c2.number_of_sig_ver_errors;
      number_of_annotations_required =
        c1.number_of_annotations_required + c2.number_of_annotations_required;
      number_of_annotations_skipped =
        c1.number_of_annotations_skipped + c2.number_of_annotations_skipped;
    }

  let serialize s =
    let open Utils_js in
    [
      spf "sig_ver_errors: %d" s.number_of_sig_ver_errors;
      spf "annotations_required: %d" s.number_of_annotations_required;
      spf "annotations_skipped: %d" s.number_of_annotations_skipped;
    ]

  let report s =
    [
      string_of_row ~indent:2 "Number of sig. ver. errors" s.number_of_sig_ver_errors;
      string_of_row ~indent:2 "Number of annotations required" s.number_of_annotations_required;
      string_of_row ~indent:2 "Number of annotations skipped" s.number_of_annotations_skipped;
    ]
end

module Codemod_exports_annotator = Codemod_annotator.Make (SignatureVerificationErrorStats)
module Acc = Acc (SignatureVerificationErrorStats)

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
  object (this)
    inherit
      Codemod_exports_annotator.mapper
        ~max_type_size
        ~exact_by_default
        ~lint_severities
        ~suppress_types
        ~imports_react
        ~preserve_literals
        ~default_any
        cctx as super

    (* initialized in this#program *)
    val mutable sig_verification_loc_tys = LMap.empty

    val mutable total_errors = 0

    method private annotate_expr loc expression ty =
      let open Ast.Expression in
      match expression with
      | (arrow_loc, ArrowFunction _func) ->
        this#update_acc (fun acc -> Acc.warn acc arrow_loc Warning.Skipping_arrow_function);
        Ok expression
      | (expr_loc, _) ->
        Acc.debug expr_loc (Debug.Add_annotation Debug.Expr);
        this#annotate_node loc ty (fun annot ->
            (expr_loc, TypeCast TypeCast.{ expression; annot; comments = None }))

    method private annotate_class_prop loc prop ty =
      let open Ast.Class.Property in
      this#annotate_node loc ty (fun type_ast -> { prop with annot = Ast.Type.Available type_ast })

    method! variable_declarator ~kind decl =
      let open Flow_ast.Statement.VariableDeclaration.Declarator in
      match (kind, decl) with
      | ( Ast.Statement.VariableDeclaration.Const,
          ( dloc,
            {
              id =
                ( id_loc,
                  Ast.Pattern.Identifier
                    {
                      Ast.Pattern.Identifier.name;
                      annot = Ast.Type.Missing _ as annot;
                      optional = false as optional;
                    } );
              init = Some (eloc, _) as init;
            } ) )
        when LMap.mem eloc sig_verification_loc_tys ->
        let ty = LMap.find eloc sig_verification_loc_tys in
        let f loc _annot ty = this#annotate_node loc ty (fun a -> Ast.Type.Available a) in
        let error _ = Ast.Type.Available (Loc.none, flowfixme_ast) in
        let annot' = this#opt_annotate ~f ~error ~expr:init eloc ty annot in
        (* A toplevel annotation has been added. No need to descend into init. *)
        if annot == annot' then
          decl
        else
          let decl' =
            {
              id =
                ( id_loc,
                  Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; annot = annot'; optional }
                );
              init;
            }
          in
          (dloc, decl')
      | _ -> super#variable_declarator ~kind decl

    method! private expression (expr : (Loc.t, Loc.t) Ast.Expression.t) =
      let expr = super#expression expr in
      let (loc, _) = expr in
      match LMap.find_opt loc sig_verification_loc_tys with
      | Some type_entry ->
        let f = this#annotate_expr in
        let error e =
          let (loc, _) = e in
          ( loc,
            Ast.Expression.TypeCast
              Ast.Expression.TypeCast.
                { expression = e; annot = (Loc.none, flowfixme_ast); comments = None } )
        in
        this#opt_annotate ~f ~error ~expr:(Some expr) loc type_entry expr
      | None -> expr

    method! type_annotation_hint (return : (Loc.t, Loc.t) Ast.Type.annotation_or_hint) =
      let open Ast.Type in
      match return with
      | Available _ -> return
      | Missing loc ->
        (match LMap.find_opt loc sig_verification_loc_tys with
        | None -> return
        | Some ty -> this#add_annot_to_missing loc ty return)

    method private add_annot_to_missing loc ty (return : (Loc.t, Loc.t) Ast.Type.annotation_or_hint)
        =
      let open Ast.Type in
      let f loc _annot ty = this#annotate_node loc ty (fun a -> Available a) in
      let error _ = Available (Loc.none, flowfixme_ast) in
      this#opt_annotate ~f ~error ~expr:None loc ty return

    method! class_extends loc (extends : ('loc, 'loc) Ast.Class.Extends.t') =
      match extends with
      | { Ast.Class.Extends.expr = (_, Ast.Expression.Call _); targs = None; _ } ->
        super#class_extends loc extends
      | _ ->
        (* Else skip *)
        if LMap.mem loc sig_verification_loc_tys then
          wont_annotate_locs <- LSet.add loc wont_annotate_locs;
        extends

    method! function_param_pattern (expr : (Loc.t, Loc.t) Ast.Pattern.t) =
      let open Ast.Pattern in
      match expr with
      | (loc, Identifier ({ Identifier.annot = Ast.Type.Missing _mloc as annot; _ } as id)) ->
        if default_any then
          match LMap.find_opt loc sig_verification_loc_tys with
          | None -> expr
          | Some type_entry ->
            let annot = this#add_annot_to_missing loc type_entry annot in
            (loc, Identifier { id with Identifier.annot })
        else (
          (* These will most likely be reported by the missing input annotation check *)
          if LMap.mem loc sig_verification_loc_tys then
            wont_annotate_locs <- LSet.add loc wont_annotate_locs;
          super#function_param_pattern expr
        )
      | _ -> super#function_param_pattern expr

    method! class_element (elem : (Loc.t, Loc.t) Ast.Class.Body.element) =
      let elem = super#class_element elem in
      match elem with
      | Ast.Class.Body.PrivateField (_loc, _field) -> elem (* TODO *)
      | Ast.Class.Body.Method (_loc, _meth) -> elem
      | Ast.Class.Body.Property (loc, prop) ->
        (match LMap.find_opt loc sig_verification_loc_tys with
        | None -> elem
        | Some ty ->
          let f = this#annotate_class_prop in
          let error p =
            { p with Ast.Class.Property.annot = Ast.Type.Available (Loc.none, flowfixme_ast) }
          in
          let prop' = this#opt_annotate ~f ~error ~expr:None loc ty prop in
          if prop == prop' then
            elem
          else
            Ast.Class.Body.Property (loc, prop'))

    method! variable_declarator_pattern ~kind (expr : (Loc.t, Loc.t) Flow_ast.Pattern.t) =
      let open Flow_ast.Pattern in
      let (loc, patt) = expr in
      match (patt, kind) with
      (* Unsupported cases. *)
      | ((Expression _ | Object _ | Array _), _)
      (* Annotation is present *)
      | (Identifier { Identifier.annot = Ast.Type.Available _; _ }, _)
      (* In `const x = exp;` the error appears on exp, so it's handled elsewhere. *)
      | ( Identifier { Identifier.annot = Ast.Type.Missing _; _ },
          Ast.Statement.VariableDeclaration.Const ) ->
        super#variable_declarator_pattern ~kind expr
      | ( Identifier { Identifier.name; annot = Ast.Type.Missing _ as annot; optional },
          Ast.Statement.VariableDeclaration.(Var | Let) ) ->
        let (name_loc, _) = name in
        (match LMap.find_opt name_loc sig_verification_loc_tys with
        | None -> super#variable_declarator_pattern ~kind expr
        | Some ty ->
          let name' = this#pattern_identifier ~kind name in
          let annot' = this#add_annot_to_missing loc ty annot in
          let patt' =
            if name == name' && annot == annot' then
              patt
            else
              Identifier { Identifier.name = name'; annot = annot'; optional }
          in
          if patt == patt' then
            expr
          else
            (loc, patt'))

    (* Matches arrows of the form `x => x` where the parameter missing annotation
     * position is the same as the return one. *)
    method! arrow_function loc (expr : (Loc.t, Loc.t) Ast.Function.t) =
      let open Ast.Function in
      match expr with
      | {
       params =
         ( _,
           {
             Params.params =
               [
                 ( _,
                   {
                     Param.argument =
                       ( _,
                         Ast.Pattern.Identifier
                           { Ast.Pattern.Identifier.annot = Ast.Type.Missing ploc; _ } );
                     _;
                   } );
               ];
             rest = None;
             comments = _;
           } );
       return = Ast.Type.Missing rloc;
       _;
      }
        when ploc = rloc ->
        if LMap.mem rloc sig_verification_loc_tys then (
          wont_annotate_locs <- LSet.add rloc wont_annotate_locs;
          this#update_acc (fun acc -> Acc.warn acc loc Warning.Skipping_arrow_function)
        );
        expr
      | _ -> this#function_ loc expr

    method private post_run () =
      let not_annotated_locs =
        LMap.fold
          (fun loc _ acc ->
            if LMap.mem loc added_annotations_locmap then
              (* we added an annot *)
              acc
            else if LSet.mem loc wont_annotate_locs then
              (* we are explicitly avoiding it *)
              acc
            else if LSet.mem loc codemod_error_locs then
              (* codemod error *)
              acc
            else
              loc :: acc)
          sig_verification_loc_tys
          []
      in
      List.iter
        (fun loc -> this#update_acc (fun acc -> Acc.warn acc loc Warning.Location_unhandled))
        not_annotated_locs;
      let stats =
        {
          SignatureVerificationErrorStats.number_of_sig_ver_errors = total_errors;
          number_of_annotations_required = LMap.cardinal sig_verification_loc_tys;
          number_of_annotations_skipped = LSet.cardinal wont_annotate_locs;
        }
      in
      stats

    method! program prog =
      let (total_errors_, sig_verification_loc_tys_) =
        SignatureVerification.collect_annotations
          cctx
          ~preserve_literals
          ~default_any
          ~max_type_size
          prog
      in
      total_errors <- total_errors_;
      sig_verification_loc_tys <- sig_verification_loc_tys_;
      if LMap.is_empty sig_verification_loc_tys then
        (* short when no signature *)
        prog
      else
        super#program prog
  end
