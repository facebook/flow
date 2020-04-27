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
module ExportsHelper = Annotate_exports_imports
module Hardcoded_Ty_Fixes = Annotate_exports_hardcoded_ty_fixes
open Annotate_exports_utils

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

(* Types with more nodes than this number will cause a warning. *)
let type_size_warning_threshold = 30

let norm_opts =
  {
    Ty_normalizer_env.fall_through_merged = false;
    expand_internal_types = false;
    expand_type_aliases = false;
    flag_shadowed_type_params = false;
    preserve_inferred_literal_types = true;
    evaluate_type_destructors = false;
    optimize_types = false;
    omit_targ_defaults = true;
    merge_bot_and_any_kinds = false;
    verbose_normalizer = false;
    expand_toplevel_members = None;
    max_depth = None;
  }

module SignatureVerification = struct
  type type_entry =
    | NoErrors of Ty.t
    | WithErrors of Error.kind list * Ty.t

  let string_of_type_entry = function
    | NoErrors t -> Utils_js.spf "NoError (%s)" (Ty_printer.string_of_t t)
    | WithErrors (errs, t) ->
      Utils_js.spf
        "WithErrors ([%s], %s)"
        (List.map Error.serialize errs |> String.concat ",")
        (Ty_printer.string_of_t t)

  let supported_error_kind cctx ~max_type_size acc loc =
    let loc = ALoc.to_loc_exn loc in
    let add_ty ty =
      (* NOTE simplify before validating to avoid flagging spurious empty's,
       * eg. empty's that will be simplified away as parts of unions.
       * Do not simplify empties. Ignoring some of the attendant upper bounds
       * might lead to unsound types.
       *)
      let ty = Ty_utils.simplify_type ~merge_kinds:false ty in
      match Insert_type_utils.validate_type ~size_limit:max_type_size ty with
      | (ty, []) -> NoErrors ty
      | (ty, errs) ->
        let errs = List.map (fun e -> Error.Validation_error e) errs in
        WithErrors (errs, ty)
    in
    let type_entry =
      match Codemod_context.Typed.ty_at_loc norm_opts cctx loc with
      | Ok (Ty.Type ty) -> add_ty ty
      | Ok (Ty.Decl (Ty.ClassDecl (s, _))) -> add_ty (Ty.TypeOf (Ty.TSymbol s))
      | Ok _ ->
        let ty = Ty.explicit_any in
        let errors = [Error.Missing_annotation_or_normalizer_error] in
        WithErrors (errors, ty)
      | Error _ ->
        let ty = Ty.explicit_any in
        let errors = [Error.Missing_annotation_or_normalizer_error] in
        WithErrors (errors, ty)
    in
    LMap.add loc type_entry acc

  let unsupported_error_kind ~default_any acc loc =
    if default_any then
      let loc = ALoc.to_loc_exn loc in
      let ty_entry = WithErrors ([Error.Unsupported_error_kind], Ty.explicit_any) in
      LMap.add loc ty_entry acc
    else
      acc

  let collect_annotations cctx ~default_any ~max_type_size file_sig =
    let tolerable_errors = file_sig.File_sig.With_ALoc.tolerable_errors in
    let (total_errors, ty_map) =
      List.fold_left
        (fun (tot_errors, acc) err ->
          match err with
          | File_sig.With_ALoc.SignatureVerificationError sve ->
            Signature_error.(
              (match sve with
              | ExpectedAnnotation (loc, _)
              | UnexpectedExpression (loc, _)
              | UnexpectedObjectKey (loc, _)
              | UnexpectedObjectSpread (loc, _)
              | EmptyArray loc
              | EmptyObject loc
              | UnexpectedArraySpread (loc, _) ->
                (tot_errors + 1, supported_error_kind cctx ~max_type_size acc loc)
              | ExpectedSort (_, _, loc)
              | InvalidTypeParamUse loc
              | SketchyToplevelDef loc
              | UnexpectedArrayHole loc
              | UnsupportedPredicateExpression loc
              | TODO (_, loc) ->
                (tot_errors + 1, unsupported_error_kind ~default_any acc loc)))
          | _ -> (tot_errors, acc))
        (0, LMap.empty)
        tolerable_errors
    in
    (total_errors, ty_map)
end

module Queries = struct
  class ident_visitor ~init =
    object (_this)
      inherit [SSet.t ref, Loc.t] Flow_ast_visitor.visitor ~init

      method! identifier id =
        let (_, { Ast.Identifier.name; _ }) = id in
        init := SSet.add name !init;
        id
    end

  let used_names prog =
    let idents = ref SSet.empty in
    let visitor = new ident_visitor idents in
    let _ = visitor#program prog in
    !idents
end

type ty_or_type_ast =
  | Ty_ of Ty.t
  | Type_ast of Annotate_exports_hardcoded_expr_fixes.hard_coded_type_ast

module NSpecSet = Set.Make (struct
  type t = (Loc.t, Loc.t) Ast.Statement.ImportDeclaration.named_specifier

  let compare = Pervasives.compare
end)

module HardCodedImportMap = struct
  include WrappedMap.Make (struct
    type t = Loc.t * Loc.t Annotate_exports_utils.Ast.StringLiteral.t

    let compare = Pervasives.compare
  end)

  let to_import_stmts m =
    bindings m
    |> Base.List.map ~f:(fun (source, nspecs) ->
           let nspecs = NSpecSet.elements nspecs in
           ( Loc.none,
             Ast.Statement.ImportDeclaration
               {
                 Ast.Statement.ImportDeclaration.importKind =
                   Ast.Statement.ImportDeclaration.ImportType;
                 source;
                 default = None;
                 specifiers = Some (Ast.Statement.ImportDeclaration.ImportNamedSpecifiers nspecs);
                 comments = Flow_ast_utils.mk_comments_opt ();
               } ))
end

let mapper ~preserve_literals ~max_type_size ~default_any (cctx : Codemod_context.Typed.t) =
  let { Codemod_context.Typed.file; file_sig; metadata; options; _ } = cctx in
  let imports_react = Annotate_exports_imports.ImportsHelper.imports_react file_sig in
  let (total_errors, sig_verification_loc_tys) =
    SignatureVerification.collect_annotations cctx ~default_any ~max_type_size file_sig
  in
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
  let flowfixme_ast = Builtins.flowfixme_ast lint_severities suppress_types in
  object (this)
    inherit [Acc.t, Loc.t] Flow_ast_visitor.visitor ~init:Acc.empty as super

    val mutable added_annotations_locmap = LMap.empty

    val mutable wont_annotate_locs = LSet.empty

    val mutable codemod_error_locs = LSet.empty

    val mutable remote_converter = None

    val mutable hardcoded_imports = HardCodedImportMap.empty

    method private get_remote_converter = Base.Option.value_exn remote_converter

    method private serialize t =
      match Ty_serializer.type_ t with
      | Error e -> Error (Error.Serializer_error e)
      | Ok t -> Ok t

    (* This one does the actual annotation *)
    method private with_serial
        : 'a. Loc.t -> ty_or_type_ast -> (Loc.t * (Loc.t, Loc.t) Ast.Type.t -> 'a) ->
          ('a, Error.kind) result =
      let run loc ty =
        let (acc', ty) =
          Hardcoded_Ty_Fixes.run
            ~cctx
            ~lint_severities
            ~suppress_types
            ~imports_react
            ~preserve_literals
            acc
            loc
            ty
        in
        this#set_acc acc';
        let%bind ty = this#get_remote_converter#type_ ty in
        this#serialize ty
      in
      fun loc ty f ->
        match ty with
        | Ty_ ty ->
          begin
            match run loc ty with
            | Ok t_ast ->
              let size = Ty_utils.size_of_type ~max:max_type_size ty in
              let t_ast' = Insert_type_utils.patch_up_type_ast t_ast in
              added_annotations_locmap <- LMap.add loc size added_annotations_locmap;
              Ok (f (Loc.none, t_ast'))
            | Error e ->
              this#update_acc (fun acc -> Acc.error acc loc e);
              codemod_error_locs <- LSet.add loc codemod_error_locs;
              Error e
          end
        | Type_ast { Annotate_exports_hardcoded_expr_fixes.tast_type = t; tast_imports } ->
          let size = Some 1 (* TODO *) in
          added_annotations_locmap <- LMap.add loc size added_annotations_locmap;
          List.iter
            (fun (source, nspec) ->
              hardcoded_imports <-
                HardCodedImportMap.add
                  ~combine:NSpecSet.union
                  source
                  (NSpecSet.singleton nspec)
                  hardcoded_imports)
            tast_imports;
          Ok (f (Loc.none, t))

    method private annotate_expr loc expression ty =
      let open Ast.Expression in
      match expression with
      | (arrow_loc, ArrowFunction _func) ->
        this#update_acc (fun acc -> Acc.warn acc arrow_loc Warning.Skipping_arrow_function);
        Ok expression
      | (expr_loc, _) ->
        Acc.debug expr_loc (Debug.Add_annotation Debug.Expr);
        this#with_serial loc ty (fun annot ->
            ( expr_loc,
              TypeCast TypeCast.{ expression; annot; comments = Flow_ast_utils.mk_comments_opt () }
            ))

    method private annotate_class_prop loc prop ty =
      let open Ast.Class.Property in
      this#with_serial loc ty (fun type_ast -> { prop with annot = Ast.Type.Available type_ast })

    method private opt_annotate_inferred_type
        : 'a. f:(Loc.t -> 'a -> ty_or_type_ast -> ('a, Error.kind) result) -> error:('a -> 'a) ->
          Loc.t -> ty_or_type_ast -> 'a -> 'a =
      fun ~f ~error loc ty x ->
        match f loc x ty with
        | Ok y ->
          Acc.debug loc (Debug.Add_annotation Debug.Prop);
          y
        | Error e when default_any ->
          this#update_acc (fun acc -> Acc.error acc loc e);
          codemod_error_locs <- LSet.add loc codemod_error_locs;
          let _desc = Error.serialize e in
          Acc.info loc Info.Default_any;
          error x
        | Error _ -> x

    (* The 'expr' parameter is used for hard-coding type annotations on expressions
     * matching annotate_exports_hardcoded_expr_fixes.expr_to_type_ast.
     *)
    method private opt_annotate
        : 'a. f:(Loc.t -> 'a -> ty_or_type_ast -> ('a, Error.kind) result) -> error:('a -> 'a) ->
          expr:(Loc.t, Loc.t) Ast.Expression.t option -> Loc.t ->
          SignatureVerification.type_entry -> 'a -> 'a =
      fun ~f ~error ~expr loc ty_entry x ->
        let hard_coded_ast_type =
          match expr with
          | Some expr -> Annotate_exports_hardcoded_expr_fixes.expr_to_type_ast expr
          | None -> None
        in
        match (hard_coded_ast_type, ty_entry) with
        | (Some type_ast, _) -> this#opt_annotate_inferred_type ~f ~error loc (Type_ast type_ast) x
        | (None, SignatureVerification.WithErrors (errs, ty)) ->
          List.iter (fun err -> this#update_acc (fun acc -> Acc.error acc loc err)) errs;
          codemod_error_locs <- LSet.add loc codemod_error_locs;
          if default_any then (
            Acc.info loc Info.Default_any;
            this#opt_annotate_inferred_type ~f ~error loc (Ty_ ty) x
          ) else
            x
        | (None, SignatureVerification.NoErrors ty) ->
          this#opt_annotate_inferred_type ~f ~error loc (Ty_ ty) x

    (* Copied from here: /facebook/compiler/utils/transform_utils.ml
     * Was having trouble importing due to buck.
     *)
    method private is_directive_statement (stmt : (Loc.t, Loc.t) Ast.Statement.t) =
      let open Ast.Statement in
      match stmt with
      | (_loc, Expression { Expression.directive = Some _; _ })
      | (_loc, ImportDeclaration { ImportDeclaration.importKind = ImportDeclaration.ImportType; _ })
        ->
        true
      | _ -> false

    method private add_statement_after_directive_and_type_imports
        (block_stmts : (Loc.t, Loc.t) Ast.Statement.t list)
        (insert_stmts : (Loc.t, Loc.t) Ast.Statement.t list) =
      match block_stmts with
      | [] -> insert_stmts
      | stmt :: block when this#is_directive_statement stmt ->
        (* TODO make tail-recursive *)
        stmt :: this#add_statement_after_directive_and_type_imports block insert_stmts
      | _ -> insert_stmts @ block_stmts

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
        let f loc _annot ty = this#with_serial loc ty (fun a -> Ast.Type.Available a) in
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
                {
                  expression = e;
                  annot = (Loc.none, flowfixme_ast);
                  comments = Flow_ast_utils.mk_comments_opt ();
                } )
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
      let f loc _annot ty = this#with_serial loc ty (fun a -> Available a) in
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

    method! program prog =
      if LMap.is_empty sig_verification_loc_tys then
        (* short when no signature *)
        prog
      else
        (* Gather used identifier names *)
        let reserved_names = Queries.used_names prog in
        remote_converter <-
          Some
            (new Annotate_exports_imports.ImportsHelper.remote_converter
               ~iteration:cctx.Codemod_context.Typed.iteration
               ~file
               ~reserved_names);

        let prog' = super#program prog in
        let (loc, stmts, comments) = prog' in

        if prog != prog' then
          this#update_acc (fun acc ->
              { acc with Acc.changed_set = Utils_js.FilenameSet.add file acc.Acc.changed_set });

        (* Post run stats *)
        let total_size =
          LMap.fold
            (fun loc size total ->
              let size =
                match size with
                | Some x -> x
                | None -> max_type_size
              in
              if size > type_size_warning_threshold then
                this#update_acc (fun acc -> Acc.warn acc loc (Warning.Large_type_added size));
              total + size)
            added_annotations_locmap
            0
        in

        let stats =
          {
            Stats.number_of_sig_ver_errors = total_errors;
            number_of_annotations_required = LMap.cardinal sig_verification_loc_tys;
            number_of_annotations_added = LMap.cardinal added_annotations_locmap;
            total_size_of_annotations = total_size;
            number_of_annotations_skipped = LSet.cardinal wont_annotate_locs;
          }
        in

        Hh_logger.info "%s file stats: %s" (File_key.to_string file) (Stats.serialize stats);
        this#update_acc (fun acc -> { acc with Acc.stats });
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

        let hardcoded_imports = HardCodedImportMap.to_import_stmts hardcoded_imports in
        let inferred_imports = this#get_remote_converter#to_import_stmts () in
        let generated_imports = hardcoded_imports @ inferred_imports in
        let stmts = this#add_statement_after_directive_and_type_imports stmts generated_imports in
        (loc, stmts, comments)
  end
