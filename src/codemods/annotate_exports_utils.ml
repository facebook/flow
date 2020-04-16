(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
module Ast = Flow_ast

let ( >>| ) = Base.Result.( >>| )

let ( >>= ) = Base.Result.( >>= )

let return = Base.Result.return

let mapM f xs = Base.Result.all (Base.List.map ~f xs)

let show m =
  List.fold_left
    (fun acc (loc, syms) ->
      List.fold_left
        (fun a { Ty.sym_provenance; sym_name; _ } ->
          let x =
            spf
              "%s\n%s - %s\n"
              (Reason.string_of_loc loc)
              sym_name
              (Ty_debug.ctor_of_provenance sym_provenance)
          in
          x :: a)
        acc
        syms)
    []
    m
  |> List.rev
  |> String.concat "\n"

let combine = List.rev_append

module SymbolMap = struct
  module M = WrappedMap.Make (struct
    type t = Ty_symbol.symbol

    let compare = Pervasives.compare
  end)

  include M
end

module SymbolSet = Set.Make (struct
  type t = Ty_symbol.symbol

  let compare = Pervasives.compare
end)

let is_react_file_key = function
  | File_key.LibFile x -> Filename.basename x = "react.js"
  | _ -> false

let is_react_redux_file_key = function
  | File_key.LibFile x -> Filename.basename x = "react-redux_v5.x.x.js"
  | _ -> false

let is_react_loc loc =
  match ALoc.source loc with
  | Some f -> is_react_file_key f
  | _ -> false

let is_react_redux_loc loc =
  match ALoc.source loc with
  | Some f -> is_react_redux_file_key f
  | _ -> false

let width = 45

let string_of_row ?(indent = 0) name i =
  let len = String.length name in
  let padding = width - len - indent - 7 in
  spf "%*s%s:%*s%6d" indent " " name padding " " i

let print_section name = spf "%s:" name

module Debug = struct
  type node_kind =
    | Expr
    | Prop

  type t = Add_annotation of node_kind

  let serialize_node_kind = function
    | Expr -> "Expr"
    | Prop -> "Prop"

  let serialize = function
    | Add_annotation k -> spf "Add_annotation %s" (serialize_node_kind k)
end

module Info = struct
  type t = Default_any

  let serialize = function
    | Default_any -> "Default_any"
end

module Warning = struct
  type kind =
    | Skipping_arrow_function
    | Large_type_added of int
    | Location_unhandled
    | Empty_NoUpper
    | Empty_SomeKnownUpper

  type counts = {
    skipping_arrow_function: int;
    large_type_added: int;
    location_unhandled: int;
    empty_NoUpper: int;
    empty_SomeKnownUpper: int;
  }

  let empty =
    {
      skipping_arrow_function = 0;
      large_type_added = 0;
      location_unhandled = 0;
      empty_NoUpper = 0;
      empty_SomeKnownUpper = 0;
    }

  let combine c1 c2 =
    {
      skipping_arrow_function = c1.skipping_arrow_function + c2.skipping_arrow_function;
      large_type_added = c1.large_type_added + c2.large_type_added;
      location_unhandled = c1.location_unhandled + c2.location_unhandled;
      empty_NoUpper = c1.empty_NoUpper + c2.empty_NoUpper;
      empty_SomeKnownUpper = c1.empty_SomeKnownUpper + c2.empty_SomeKnownUpper;
    }

  let serialize = function
    | Skipping_arrow_function -> "Skipping_arrow_function"
    | Large_type_added n -> spf "Large_type_added %d" n
    | Location_unhandled -> "Location_unhandled"
    | Empty_NoUpper -> "Empty_NoUpper"
    | Empty_SomeKnownUpper -> "Empty_SomeKnownUpper"

  let report c =
    let rows =
      [
        string_of_row ~indent:2 "Skipping arrow function" c.skipping_arrow_function;
        string_of_row ~indent:2 "Large type added" c.large_type_added;
        string_of_row ~indent:2 "Location unhandled" c.location_unhandled;
        string_of_row ~indent:2 "Empty NoUpper" c.empty_NoUpper;
        string_of_row ~indent:2 "Empty SomeKnownUpper" c.empty_SomeKnownUpper;
      ]
    in
    String.concat "\n" rows

  let add c = function
    | Skipping_arrow_function -> { c with skipping_arrow_function = c.skipping_arrow_function + 1 }
    | Large_type_added _ -> { c with large_type_added = c.large_type_added + 1 }
    | Location_unhandled -> { c with location_unhandled = c.location_unhandled + 1 }
    | Empty_NoUpper -> { c with empty_NoUpper = c.empty_NoUpper + 1 }
    | Empty_SomeKnownUpper -> { c with empty_SomeKnownUpper = c.empty_SomeKnownUpper + 1 }
end

module Error = struct
  type validation_error = Insert_type_utils.validation_error

  type import_error =
    | Loc_source_none
    | Parsing_heaps_get_ast_error
    | Indeterminate_module_type
    | No_matching_export of string * ALoc.t
    | Unrecognizable_module_error of string

  type import_error_counts = {
    loc_source_none: int;
    parsing_heaps_get_ast_error: int;
    indeterminate_module_type: int;
    no_matching_export: int;
    unrecognizable_module_error: int;
  }

  type kind =
    | Missing_annotation_or_normalizer_error
    | Validation_error of validation_error
    | Import_error of import_error
    | Serializer_error of string
    | Unsupported_error_kind

  type counts = {
    missing_annotation_or_normalizer_error: int;
    validation_error: int;
    import_error: import_error_counts;
    serializer_error: int;
    unsupported_error_kind: int;
  }

  let empty =
    {
      missing_annotation_or_normalizer_error = 0;
      validation_error = 0;
      import_error =
        {
          loc_source_none = 0;
          parsing_heaps_get_ast_error = 0;
          indeterminate_module_type = 0;
          no_matching_export = 0;
          unrecognizable_module_error = 0;
        };
      serializer_error = 0;
      unsupported_error_kind = 0;
    }

  let combine_import_errors c1 c2 =
    {
      loc_source_none = c1.loc_source_none + c2.loc_source_none;
      parsing_heaps_get_ast_error = c1.parsing_heaps_get_ast_error + c2.parsing_heaps_get_ast_error;
      indeterminate_module_type = c1.indeterminate_module_type + c2.indeterminate_module_type;
      no_matching_export = c1.no_matching_export + c2.no_matching_export;
      unrecognizable_module_error = c1.unrecognizable_module_error + c2.unrecognizable_module_error;
    }

  let combine c1 c2 =
    {
      missing_annotation_or_normalizer_error =
        c1.missing_annotation_or_normalizer_error + c2.missing_annotation_or_normalizer_error;
      validation_error = c1.validation_error + c2.validation_error;
      import_error = combine_import_errors c1.import_error c2.import_error;
      serializer_error = c1.serializer_error + c2.serializer_error;
      unsupported_error_kind = c1.unsupported_error_kind + c2.unsupported_error_kind;
    }

  let serialize_import_error = function
    | Loc_source_none -> "Loc_source_none"
    | Parsing_heaps_get_ast_error -> "Parsing_heaps_get_ast_error"
    | Indeterminate_module_type -> "Indeterminate_module_type"
    | No_matching_export (x, loc) -> spf "No_matching_export (%s, %s)" x (Reason.string_of_aloc loc)
    | Unrecognizable_module_error _ -> "Unrecognizable_module_error"

  let serialize = function
    | Missing_annotation_or_normalizer_error -> "Missing_annotation_or_normalizer_error"
    | Validation_error e -> "Validation_error " ^ Insert_type_utils.serialize_validation_error e
    | Import_error e -> "Import_error " ^ serialize_import_error e
    | Serializer_error x -> "Serializer_error " ^ x
    | Unsupported_error_kind -> "Unsupported_error_kind"

  let report c =
    let rows =
      [
        string_of_row
          ~indent:2
          "Missing annot./normalizer error"
          c.missing_annotation_or_normalizer_error;
        string_of_row ~indent:2 "Validation Error" c.validation_error;
        "  Import Error:";
        string_of_row ~indent:4 "Loc source none" c.import_error.loc_source_none;
        string_of_row
          ~indent:4
          "Parsing heaps get ast error"
          c.import_error.parsing_heaps_get_ast_error;
        string_of_row ~indent:4 "Indeterminate module type" c.import_error.indeterminate_module_type;
        string_of_row ~indent:4 "No matching export" c.import_error.no_matching_export;
        string_of_row
          ~indent:4
          "Unrecognizable module error"
          c.import_error.unrecognizable_module_error;
        string_of_row ~indent:2 "Serializer error" c.serializer_error;
        string_of_row ~indent:2 "Unsupported error kind" c.unsupported_error_kind;
      ]
    in
    String.concat "\n" rows

  let add_import_error c = function
    | Loc_source_none -> { c with loc_source_none = c.loc_source_none + 1 }
    | Parsing_heaps_get_ast_error ->
      { c with parsing_heaps_get_ast_error = c.parsing_heaps_get_ast_error + 1 }
    | Indeterminate_module_type ->
      { c with indeterminate_module_type = c.indeterminate_module_type + 1 }
    | No_matching_export _ -> { c with no_matching_export = c.no_matching_export + 1 }
    | Unrecognizable_module_error _ ->
      { c with unrecognizable_module_error = c.unrecognizable_module_error + 1 }

  let add c = function
    | Missing_annotation_or_normalizer_error ->
      {
        c with
        missing_annotation_or_normalizer_error = c.missing_annotation_or_normalizer_error + 1;
      }
    | Validation_error _ -> { c with validation_error = c.validation_error + 1 }
    | Import_error e -> { c with import_error = add_import_error c.import_error e }
    | Serializer_error _ -> { c with serializer_error = c.serializer_error + 1 }
    | Unsupported_error_kind -> { c with unsupported_error_kind = c.unsupported_error_kind + 1 }
end

module Stats = struct
  type t = {
    number_of_sig_ver_errors: int;
    number_of_annotations_required: int;
    number_of_annotations_added: int;
    total_size_of_annotations: int;
    number_of_annotations_skipped: int;
  }

  let empty =
    {
      number_of_sig_ver_errors = 0;
      number_of_annotations_required = 0;
      number_of_annotations_added = 0;
      total_size_of_annotations = 0;
      number_of_annotations_skipped = 0;
    }

  let combine c1 c2 =
    {
      number_of_sig_ver_errors = c1.number_of_sig_ver_errors + c2.number_of_sig_ver_errors;
      number_of_annotations_required =
        c1.number_of_annotations_required + c2.number_of_annotations_required;
      number_of_annotations_added = c1.number_of_annotations_added + c2.number_of_annotations_added;
      total_size_of_annotations = c1.total_size_of_annotations + c2.total_size_of_annotations;
      number_of_annotations_skipped =
        c1.number_of_annotations_skipped + c2.number_of_annotations_skipped;
    }

  let serialize s =
    let open Utils_js in
    let stats =
      [
        spf "sig_ver_errors: %d" s.number_of_sig_ver_errors;
        spf "annotations_required: %d" s.number_of_annotations_required;
        spf "annotations_added: %d" s.number_of_annotations_added;
        spf "total_size_of_annotations: %d" s.total_size_of_annotations;
        spf "annotations_skipped: %d" s.number_of_annotations_skipped;
      ]
    in
    spf "(%s)" (String.concat ", " stats)

  let report s =
    let rows =
      [
        string_of_row ~indent:2 "Number of sig. ver. errors" s.number_of_sig_ver_errors;
        string_of_row ~indent:2 "Number of annotations required" s.number_of_annotations_required;
        string_of_row ~indent:2 "Number of annotations added" s.number_of_annotations_added;
        string_of_row ~indent:2 "Total size of annotations" s.total_size_of_annotations;
        string_of_row ~indent:2 "Number of annotations skipped" s.number_of_annotations_skipped;
      ]
    in
    String.concat "\n" rows
end

module Acc = struct
  type t = {
    changed_set: FilenameSet.t;
    stats: Stats.t;
    errors: Error.counts;
    warnings: Warning.counts;
  }

  let empty =
    {
      changed_set = FilenameSet.empty;
      stats = Stats.empty;
      errors = Error.empty;
      warnings = Warning.empty;
    }

  let combine c1 c2 =
    {
      changed_set = FilenameSet.union c1.changed_set c2.changed_set;
      stats = Stats.combine c1.stats c2.stats;
      errors = Error.combine c1.errors c2.errors;
      warnings = Warning.combine c1.warnings c2.warnings;
    }

  let debug loc (x : Debug.t) =
    Hh_logger.debug "%s %s" (Debug.serialize x) (Reason.string_of_loc loc)

  let info loc (x : Info.t) = Hh_logger.info "%s %s" (Info.serialize x) (Reason.string_of_loc loc)

  let warn c loc (x : Warning.kind) =
    Hh_logger.warn "%s %s" (Warning.serialize x) (Reason.string_of_loc loc);
    { c with warnings = Warning.add c.warnings x }

  let error c loc (x : Error.kind) =
    Hh_logger.error "%s %s" (Error.serialize x) (Reason.string_of_loc loc);
    { c with errors = Error.add c.errors x }

  let report ~strip_root:_ c =
    let sep = "" in
    let rows =
      [
        print_section "Stats";
        string_of_row ~indent:2 "Files changed" (FilenameSet.cardinal c.changed_set);
        Stats.report c.stats;
        sep;
        print_section "Errors";
        Error.report c.errors;
        sep;
        print_section "Warnings";
        Warning.report c.warnings;
      ]
    in
    String.concat "\n" rows
end

(* Builtin types *)

module Builtins = struct
  let flowfixme = Ty.Generic (Ty_symbol.builtin_symbol "$FlowFixMe", Ty.TypeAliasKind, None)

  let flowfixme_ast =
    match Ty_serializer.type_ flowfixme with
    | Ok ast -> ast
    | Error e -> failwith e

  let flowfixme_empty =
    Ty.Generic (Ty_symbol.builtin_symbol "$FlowFixMeEmpty", Ty.TypeAliasKind, None)

  let empty = Ty.Bot Ty.EmptyType
end
