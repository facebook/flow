(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
            Utils_js.spf
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

module Debug = struct
  type node_kind =
    | Expr
    | Prop

  type t = Add_annotation of node_kind

  let serialize_node_kind = function
    | Expr -> "Expr"
    | Prop -> "Prop"

  let serialize = function
    | Add_annotation k -> Utils_js.spf "Add_annotation %s" (serialize_node_kind k)
end

module Info = struct
  type t = Default_any

  let serialize = function
    | Default_any -> "Default_any"
end

module Warning = struct
  type t =
    | Skipping_arrow_function
    | Large_type_added of int
    | Location_unhandled
    | Empty_NoUpper
    | Empty_SomeKnownUpper

  let serialize = function
    | Skipping_arrow_function -> "Skipping_arrow_function"
    | Large_type_added n -> Utils_js.spf "Large_type_added %d" n
    | Location_unhandled -> "Location_unhandled"
    | Empty_NoUpper -> "Empty_NoUpper"
    | Empty_SomeKnownUpper -> "Empty_SomeKnownUpper"
end

module Errors = struct
  type validation_error = Insert_type_utils.validation_error

  type import_error =
    | Loc_source_none
    | Parsing_heaps_get_ast_error
    | Indeterminate_module_type
    | No_matching_export of string * ALoc.t
    | Unrecognizable_module_error of string

  type t =
    | Missing_annotation_or_normalizer_error
    | Validation_error of validation_error
    | Import_error of import_error
    | Serializer_error of string
    | Unsupported_error_kind

  let serialize_import_error = function
    | Loc_source_none -> "Loc_source_none"
    | Parsing_heaps_get_ast_error -> "Parsing_heaps_get_ast_error"
    | Indeterminate_module_type -> "Indeterminate_module_type"
    | No_matching_export (x, loc) ->
      Utils_js.spf "No_matching_export (%s, %s)" x (Reason.string_of_aloc loc)
    | Unrecognizable_module_error _ -> "Unrecognizable_module_error"

  let serialize = function
    | Missing_annotation_or_normalizer_error -> "Missing_annotation_or_normalizer_error"
    | Validation_error e -> "Validation_error " ^ Insert_type_utils.serialize_validation_error e
    | Import_error e -> "Import_error " ^ serialize_import_error e
    | Serializer_error x -> "Serializer_error " ^ x
    | Unsupported_error_kind -> "Unsupported_error_kind"
end

module Stats = struct
  type t =
    | Number_of_sig_ver_errors of int
    | Number_of_annotations_required of int
    | Number_of_annotations_added of int
    | Total_size_of_annotations of int
    | Number_of_annotations_skipped of int

  let serialize = function
    | Number_of_sig_ver_errors n -> Utils_js.spf "Number_of_sig_ver_errors %d" n
    | Number_of_annotations_required n -> Utils_js.spf "Number_of_annotations_required %d" n
    | Number_of_annotations_added n -> Utils_js.spf "Number_of_annotations_added %d" n
    | Total_size_of_annotations n -> Utils_js.spf "Total_size_of_annotations %d" n
    | Number_of_annotations_skipped n -> Utils_js.spf "Number_of_annotations_skipped %d" n
end

module Logger = struct
  let debug loc (x : Debug.t) =
    Hh_logger.debug "%s %s" (Debug.serialize x) (Reason.string_of_loc loc)

  let info loc (x : Info.t) = Hh_logger.info "%s %s" (Info.serialize x) (Reason.string_of_loc loc)

  let warn loc (x : Warning.t) =
    Hh_logger.warn "%s %s" (Warning.serialize x) (Reason.string_of_loc loc)

  let error loc (x : Errors.t) =
    Hh_logger.error "%s %s" (Errors.serialize x) (Reason.string_of_loc loc)

  let stats file (x : Stats.t) =
    Hh_logger.info "%s %s" (Stats.serialize x) (File_key.to_string file)
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
