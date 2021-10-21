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
open Insert_type_utils

module Let_syntax = struct
  let return = return

  let bind x ~f = x >>= f

  let map x ~f = x >>| f
end

(* Types with more nodes than this number will cause a warning. *)
let type_size_warning_threshold = 30

module Queries = struct
  class ident_visitor ~init =
    object (this)
      inherit [SSet.t ref, Loc.t] Flow_ast_visitor.visitor ~init

      (* Skip keys, qualified identifiers *)
      method! object_key_identifier ident = ident

      method! member_property_identifier ident = ident

      method! generic_qualified_identifier_type qual =
        let open Ast.Type.Generic.Identifier in
        let (loc, { qualification; id }) = qual in
        let qualification' = this#generic_identifier_type qualification in
        (* Skips the id part *)
        if qualification' == qualification then
          qual
        else
          (loc, { qualification = qualification'; id })

      method! identifier id =
        let (_, { Ast.Identifier.name; _ }) = id in
        init := SSet.add name !init;
        id

      method! jsx_identifier id =
        let (_, { Ast.JSX.Identifier.name; _ }) = id in
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

module NSpecSet = Flow_set.Make (struct
  type t = (Loc.t, Loc.t) Ast.Statement.ImportDeclaration.named_specifier

  let compare = Stdlib.compare
end)

module HardCodedImportMap = struct
  include WrappedMap.Make (struct
    type t = Loc.t * Loc.t Ast.StringLiteral.t

    let compare = Stdlib.compare
  end)

  let to_import_stmts m =
    bindings m
    |> Base.List.map ~f:(fun (source, nspecs) ->
           let nspecs = NSpecSet.elements nspecs in
           ( Loc.none,
             Ast.Statement.ImportDeclaration
               {
                 Ast.Statement.ImportDeclaration.import_kind =
                   Ast.Statement.ImportDeclaration.ImportType;
                 source;
                 default = None;
                 specifiers = Some (Ast.Statement.ImportDeclaration.ImportNamedSpecifiers nspecs);
                 comments = None;
               }
           )
       )
end

let validate_ty cctx ~max_type_size ty =
  let reader = cctx.Codemod_context.Typed.reader in
  let loc_of_aloc = Parsing_heaps.Reader_dispatcher.loc_of_aloc ~reader in
  (* NOTE simplify before validating to avoid flagging spurious empty's,
   * eg. empty's that will be simplified away as parts of unions.
   * Do not simplify empties. Ignoring some of the attendant upper bounds
   * might lead to unsound types. *)
  let ty = Ty_utils.simplify_type ~merge_kinds:false ty in
  let (ty, errs) = Validator.validate_type ~size_limit:max_type_size ~loc_of_aloc ty in
  match errs with
  | [] -> Ok ty
  | errs -> Error (List.map (fun e -> Error.Validation_error e) errs)

(* Used to infer the type for an annotation from an error loc *)
let get_ty cctx ~preserve_literals ~max_type_size loc =
  let preserve_inferred_literal_types =
    Codemod_hardcoded_ty_fixes.PreserveLiterals.(
      match preserve_literals with
      | Always
      | Auto ->
        true
      | Never -> false
    )
  in
  let norm_opts =
    {
      Ty_normalizer_env.expand_internal_types = false;
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
  match Codemod_context.Typed.ty_at_loc norm_opts cctx loc with
  | Ok (Ty.Type ty) -> validate_ty cctx ~max_type_size ty
  | Ok (Ty.Decl (Ty.ClassDecl (s, _))) -> validate_ty cctx ~max_type_size (Ty.TypeOf (Ty.TSymbol s))
  | Ok _ -> Error [Error.Missing_annotation_or_normalizer_error]
  | Error _ -> Error [Error.Missing_annotation_or_normalizer_error]

module Make (Extra : BASE_STATS) = struct
  module Stats = Stats (Extra)
  module Acc = Acc (Extra)
  module Hardcoded_Ty_Fixes = Codemod_hardcoded_ty_fixes.Make (Extra)

  class virtual mapper
    ~max_type_size
    ~exact_by_default
    ~lint_severities
    ~suppress_types
    ~imports_react
    ~preserve_literals
    ~default_any
    (cctx : Codemod_context.Typed.t) =
    object (this)
      inherit [Acc.t, Loc.t] Flow_ast_visitor.visitor ~init:Acc.empty as super

      val mutable added_annotations_locmap = LMap.empty

      val mutable wont_annotate_locs = LSet.empty

      val mutable codemod_error_locs = LSet.empty

      val mutable remote_converter = None

      val mutable hardcoded_imports = HardCodedImportMap.empty

      method private get_remote_converter = Base.Option.value_exn remote_converter

      method private serialize t =
        match Ty_serializer.(type_ { exact_by_default } t) with
        | Error e -> Error (Error.Serializer_error e)
        | Ok t -> Ok t

      method private replace_type_node_with_ty =
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
        fun loc ty ->
          match run loc ty with
          | Ok t_ast ->
            let size = Ty_utils.size_of_type ~max:max_type_size ty in
            let t_ast' = Insert_type_utils.patch_up_type_ast t_ast in
            added_annotations_locmap <- LMap.add loc size added_annotations_locmap;
            Ok t_ast'
          | Error e ->
            this#update_acc (fun acc -> Acc.error acc loc e);
            codemod_error_locs <- LSet.add loc codemod_error_locs;
            Error e

      (* This one does the actual annotation *)
      method private annotate_node
          : 'a.
            Loc.t ->
            ty_or_type_ast ->
            (Loc.t * (Loc.t, Loc.t) Ast.Type.t -> 'a) ->
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
        fun loc ty_or_type_ast f ->
          match ty_or_type_ast with
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

      method private opt_annotate_inferred_type
          : 'a.
            f:(Loc.t -> 'a -> ty_or_type_ast -> ('a, Error.kind) result) ->
            error:('a -> 'a) ->
            Loc.t ->
            ty_or_type_ast ->
            'a ->
            'a =
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
       * matching annotate_exports_hardcoded_expr_fixes.expr_to_type_ast. *)
      method private opt_annotate
          : 'a.
            f:(Loc.t -> 'a -> ty_or_type_ast -> ('a, Error.kind) result) ->
            error:('a -> 'a) ->
            expr:(Loc.t, Loc.t) Ast.Expression.t option ->
            Loc.t ->
            (Ty.t, Error.kind list) result ->
            'a ->
            'a =
        fun ~f ~error ~expr loc ty_entry x ->
          let hard_coded_ast_type =
            match expr with
            | Some expr -> Annotate_exports_hardcoded_expr_fixes.expr_to_type_ast expr
            | None -> None
          in
          match (hard_coded_ast_type, ty_entry) with
          | (Some type_ast, _) ->
            this#opt_annotate_inferred_type ~f ~error loc (Type_ast type_ast) x
          | (None, Error errs) ->
            List.iter (fun err -> this#update_acc (fun acc -> Acc.error acc loc err)) errs;
            codemod_error_locs <- LSet.add loc codemod_error_locs;
            if default_any then (
              Acc.info loc Info.Default_any;
              this#opt_annotate_inferred_type ~f ~error loc (Ty_ Ty.explicit_any) x
            ) else
              x
          | (None, Ok ty) -> this#opt_annotate_inferred_type ~f ~error loc (Ty_ ty) x

      (* Useful to annotate expressions with a typecast. Skips arrow functions *)
      method private annotate_expr loc expression ty =
        let open Ast.Expression in
        match expression with
        | (arrow_loc, ArrowFunction _func) ->
          this#update_acc (fun acc -> Acc.warn acc arrow_loc Warning.Skipping_arrow_function);
          Ok expression
        | (expr_loc, _) ->
          Acc.debug expr_loc (Debug.Add_annotation Debug.Expr);
          this#annotate_node loc ty (fun annot ->
              (expr_loc, TypeCast TypeCast.{ expression; annot; comments = None })
          )

      method private add_unannotated_loc_warnings lmap =
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
            lmap
            []
        in
        List.iter
          (fun loc -> this#update_acc (fun acc -> Acc.warn acc loc Warning.Location_unhandled))
          not_annotated_locs

      method virtual private post_run : unit -> Extra.t

      method! program prog =
        (* Gather used identifier names *)
        let reserved_names = Queries.used_names prog in
        let file = cctx.Codemod_context.Typed.file in
        remote_converter <-
          Some
            (new Insert_type_imports.ImportsHelper.remote_converter
               ~iteration:cctx.Codemod_context.Typed.iteration
               ~file
               ~reserved_names
            );

        let prog' = super#program prog in
        let (loc, { Ast.Program.statements = stmts; comments; all_comments }) = prog' in

        if prog != prog' then
          this#update_acc (fun acc ->
              { acc with Acc.changed_set = Utils_js.FilenameSet.add file acc.Acc.changed_set }
          );

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

        let extra = this#post_run () in

        let stats =
          {
            Stats.number_of_annotations_added = LMap.cardinal added_annotations_locmap;
            total_size_of_annotations = total_size;
            extra;
          }
        in
        Hh_logger.info "%s file stats: %s" (File_key.to_string file) (Stats.serialize stats);
        this#update_acc (fun acc -> { acc with Acc.stats });
        let hardcoded_imports = HardCodedImportMap.to_import_stmts hardcoded_imports in
        let inferred_imports = this#get_remote_converter#to_import_stmts () in
        let generated_imports = hardcoded_imports @ inferred_imports in
        let stmts =
          Insert_type.add_statement_after_directive_and_type_imports stmts generated_imports
        in
        (loc, { Ast.Program.statements = stmts; comments; all_comments })
    end
end
