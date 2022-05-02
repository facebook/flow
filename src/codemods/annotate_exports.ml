(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module LSet = Loc_collections.LocSet
module ALSet = Loc_collections.ALocSet
module LMap = Loc_collections.LocMap
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
  let supported_error_kind cctx ~preserve_literals ~max_type_size acc loc =
    let ty_result = Codemod_annotator.get_validated_ty cctx ~preserve_literals ~max_type_size loc in
    LMap.add loc ty_result acc

  let unsupported_error_kind ~default_any acc loc =
    if default_any then
      LMap.add loc (Error [Error.Unsupported_error_kind]) acc
    else
      acc

  let collect_annotations cctx ~preserve_literals ~default_any ~max_type_size ast =
    let { Codemod_context.Typed.options; docblock; _ } = cctx in
    let prevent_munge =
      let should_munge = Options.should_munge_underscores options in
      Docblock.preventMunge docblock || not should_munge
    in
    let sig_opts =
      {
        Type_sig_parse.type_asserts = Options.type_asserts options;
        suppress_types = Options.suppress_types options;
        munge = not prevent_munge;
        ignore_static_propTypes = true;
        facebook_keyMirror = true;
        facebook_fbt = Options.facebook_fbt options;
        max_literal_len = Options.max_literal_length options;
        exact_by_default = Options.exact_by_default options;
        module_ref_prefix = Options.haste_module_ref_prefix options;
        enable_enums = Options.enums options;
        enable_relay_integration = Options.enable_relay_integration options;
        relay_integration_module_prefix = Options.relay_integration_module_prefix options;
      }
    in
    let (sig_errors, locs, _) =
      let strict = Docblock.is_strict docblock in
      Type_sig_utils.parse_and_pack_module ~strict sig_opts None ast
    in
    List.fold_left
      (fun acc (_, err) ->
        match err with
        | Type_sig.CheckError -> acc
        | Type_sig.SigError err ->
          let open Signature_error in
          let (tot_errors, acc) = acc in
          (match err with
          | ExpectedAnnotation (loc, _)
          | UnexpectedExpression (loc, _)
          | UnexpectedObjectKey (loc, _)
          | EmptyArray loc
          | EmptyObject loc
          | UnexpectedArraySpread (loc, _) ->
            let loc = Type_sig_collections.Locs.get locs loc in
            (tot_errors + 1, supported_error_kind cctx ~preserve_literals ~max_type_size acc loc)
          | UnexpectedArrayHole loc ->
            let loc = Type_sig_collections.Locs.get locs loc in
            (tot_errors + 1, unsupported_error_kind ~default_any acc loc)))
      (0, LMap.empty)
      sig_errors
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
  let lint_severities = Codemod_context.Typed.lint_severities cctx in
  let flowfixme_ast = Codemod_context.Typed.flowfixme_ast ~lint_severities cctx in
  object (this)
    inherit
      Codemod_exports_annotator.mapper
        cctx
        ~default_any
        ~generalize_maybe:false
        ~lint_severities
        ~max_type_size
        ~preserve_literals
        ~merge_arrays:false
        () as super

    (* initialized in this#program *)
    val mutable sig_verification_loc_tys = LMap.empty

    val mutable total_errors = 0

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
                    }
                );
              init = Some (eloc, _) as init;
            }
          )
        )
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
                { expression = e; annot = (Loc.none, flowfixme_ast); comments = None }
              
          )
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
          Ast.Statement.VariableDeclaration.Const
        ) ->
        super#variable_declarator_pattern ~kind expr
      | ( Identifier { Identifier.name; annot = Ast.Type.Missing _ as annot; optional },
          Ast.Statement.VariableDeclaration.(Var | Let)
        ) ->
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
                           { Ast.Pattern.Identifier.annot = Ast.Type.Missing ploc; _ }
                       );
                     _;
                   }
                 );
               ];
             rest = None;
             this_ = None;
             comments = _;
           }
         );
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
      this#add_unannotated_loc_warnings sig_verification_loc_tys;
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
