(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Let_syntax = struct
  let return x = Some x

  let bind x ~f = Base.Option.( >>= ) x f

  let map x ~f = Base.Option.( >>| ) x f
end

module Ast = Flow_ast
module ALocIDMap = Loc_collections.ALocIDMap

module SymbolMap = WrappedMap.Make (struct
  type t = Ty_symbol.symbol

  let compare = Stdlib.compare
end)

let get_any_str s =
  if s then
    "any"
  else
    "mixed"

module ReplaceExistentialsStats = struct
  type t = {
    default_any: bool;
    number_of_anys_added: int;
  }

  let empty = { default_any = false; number_of_anys_added = 0 }

  let combine c1 c2 =
    {
      default_any = c1.default_any || c2.default_any;
      number_of_anys_added = c1.number_of_anys_added + c2.number_of_anys_added;
    }

  let serialize s =
    [
      Utils_js.spf "default_any: %s" (get_any_str s.default_any);
      Utils_js.spf "anys_added: %d" s.number_of_anys_added;
    ]

  let report s =
    let open Insert_type_utils in
    let any_str = Utils_js.spf "Number of %s added" (get_any_str s.default_any) in
    [string_of_row ~indent:2 any_str s.number_of_anys_added]
end

module Accumulator = Insert_type_utils.Acc (ReplaceExistentialsStats)
module Codemod_existentials_annotator = Codemod_annotator.Make (ReplaceExistentialsStats)

let reporter =
  {
    Codemod_report.report = Codemod_report.StringReporter Accumulator.report;
    combine = Accumulator.combine;
    empty = Accumulator.empty;
  }

type accumulator = Accumulator.t

let mapper ~default_any ~preserve_literals ~max_type_size (ask : Codemod_context.Typed.t) =
  let options = ask.Codemod_context.Typed.options in
  let exact_by_default = Options.exact_by_default options in
  let metadata =
    Context.docblock_overrides ask.Codemod_context.Typed.docblock ask.Codemod_context.Typed.metadata
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
  let imports_react =
    Insert_type_imports.ImportsHelper.imports_react ask.Codemod_context.Typed.file_sig
  in
  object (this)
    inherit
      Codemod_existentials_annotator.mapper
        ~max_type_size
        ~exact_by_default
        ~lint_severities
        ~suppress_types
        ~imports_react
        ~preserve_literals
        ~default_any
        ask as super

    val mutable num_anys_added = 0

    val norm_opts =
      let preserve_inferred_literal_types =
        Codemod_hardcoded_ty_fixes.PreserveLiterals.(
          match preserve_literals with
          | Always
          | Auto ->
            true
          | Never -> false)
      in
      { Ty_normalizer_env.default_options with Ty_normalizer_env.preserve_inferred_literal_types }

    method private post_run () =
      ReplaceExistentialsStats.{ default_any; number_of_anys_added = num_anys_added }

    method private add_any loc comments =
      let new_type =
        if default_any then
          Ty.explicit_any
        else
          Ty.Top
      in
      match this#replace_type_node_with_ty loc new_type with
      | Ok (loc, Ast.Type.Any _) ->
        num_anys_added <- num_anys_added + 1;
        Ok (loc, Ast.Type.Any comments)
      | Ok (loc, Ast.Type.Mixed _) ->
        num_anys_added <- num_anys_added + 1;
        Ok (loc, Ast.Type.Mixed comments)
      (* This case should not be used since any/mixed are hardcoded. It will drop comments. *)
      | Ok ty ->
        num_anys_added <- num_anys_added + 1;
        Ok ty
      | Error e -> Error e

    (* Returns None if the existential is not in a generic position (ie not recorded
     * in exists_instantiations table *)
    method private get_exists_instantiation_from_table ccx loc =
      let { Codemod_context.Typed.full_cx = cx; file; file_sig; typed_ast; _ } = ccx in
      let aloc = ALoc.of_loc loc in
      let%bind ts =
        ALocIDMap.find_opt (Context.make_aloc_id cx aloc) (Context.exists_instantiations cx)
      in
      let type_ = TypeUtil.union_of_ts (Reason.mk_reason Reason.RExistential aloc) ts in
      let%bind { Type.TypeScheme.tparams_rev; _ } =
        Typed_ast_utils.find_exact_match_annotation typed_ast aloc
      in
      (* Existential under type params *)
      let genv = Ty_normalizer_env.mk_genv ~full_cx:cx ~file ~file_sig ~typed_ast in
      let scheme = { Type.TypeScheme.tparams_rev; type_ } in
      match Ty_normalizer.from_scheme ~options:norm_opts ~genv scheme with
      | Ok ty -> Some (Ok ty)
      | Error e -> Some (Error (Codemod_context.Typed.NormalizationError e))

    (**
     * Try to infer type of existentials
     *)
    method! type_ (t : ('loc, 'loc) Ast.Type.t) =
      match t with
      (* React.Element<*> to React.MixedElement *)
      | ( loc,
          Ast.Type.Generic
            Ast.Type.Generic.
              {
                id =
                  Identifier.Qualified
                    ( loc'',
                      {
                        Identifier.qualification =
                          Identifier.Unqualified (_, Ast.Identifier.{ name = "React"; _ }) as
                          react_annot;
                        Identifier.id = (_, Ast.Identifier.{ name = "Element"; _ }) as elem_annot;
                      } );
                targs = Some (_, Ast.Type.TypeArgs.{ arguments = [(_, Ast.Type.Exists _)]; _ });
                comments;
              } ) ->
        let (elem_loc, elem_id) = elem_annot in
        let mixed_element =
          (elem_loc, { elem_id with Flow_ast.Identifier.name = "MixedElement" })
        in
        ( loc,
          Ast.Type.Generic
            Ast.Type.Generic.
              {
                id =
                  Identifier.Qualified
                    ( loc'',
                      { Identifier.qualification = react_annot; Identifier.id = mixed_element } );
                targs = None;
                comments;
              } )
      | (loc, Ast.Type.Exists comments) ->
        let ty =
          match this#get_exists_instantiation_from_table ask loc with
          | Some ty_result -> ty_result
          | None -> Codemod_context.Typed.ty_at_loc norm_opts ask loc
        in
        let new_type =
          match ty with
          (* For existentials that Flow cannot determine the type of, it
           * creates a generic with the name "*". This will print as an
           * existential when the codemod is run. Replace it with any/mixed. *)
          | Ok (Ty.Type (Ty.Utility Ty.Exists)) -> this#add_any loc comments
          | Ok (Ty.Type ty) ->
            begin
              match this#replace_type_node_with_ty loc ty with
              | Error _ -> this#add_any loc comments
              | Ok (_, Ast.Type.Any _) -> Ok (loc, Ast.Type.Any comments)
              | Ok (_, Ast.Type.Empty _) ->
                let new_type =
                  if default_any then
                    Ast.Type.Any comments
                  else
                    Ast.Type.Mixed comments
                in
                Ok (loc, new_type)
              | Ok new_type -> Ok new_type
            end
          | _ -> this#add_any loc comments
        in
        (match new_type with
        | Ok ty -> ty
        | Error _ -> t)
      | _ -> super#type_ t
  end

let visit ~default_any ~preserve_literals ~max_type_size =
  Codemod_utils.make_visitor
    (Codemod_utils.Mapper (mapper ~default_any ~preserve_literals ~max_type_size))
