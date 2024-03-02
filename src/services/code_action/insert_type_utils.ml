(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

let combine = List.rev_append

module SymbolMap = struct
  module M = WrappedMap.Make (struct
    type t = Ty_symbol.symbol

    let compare = Stdlib.compare
  end)

  include M
end

module SymbolSet = Flow_set.Make (struct
  type t = Ty_symbol.symbol

  let compare = Stdlib.compare
end)

let is_react_file_key = function
  | File_key.LibFile x -> Filename.basename x = "react.js"
  | _ -> false

let is_react_redux_file_key = function
  | File_key.LibFile x -> Base.String.is_substring ~substring:"react-redux" (Filename.basename x)
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
  type validation_error =
    | TooBig of {
        size_limit: int;
        size: int option;
      }
    | Anonymous of Loc.t
    | Any_Unsound of Ty.unsoundness_kind
    | Recursive
    | ReactElementConfigFunArg
    | Empty_MatchingPropT
    | Empty_TypeDestructorTriggerT of Loc.t
    | Empty_SomeUnknownUpper of string

  let serialize_validation_error = function
    | TooBig _ -> "TooBig"
    | Anonymous loc -> Utils_js.spf "Anonymous (def: %s)" (Loc.to_string_no_source loc)
    | Any_Unsound kind -> Utils_js.spf "Any_Unsound %s" (Ty_debug.dump_any_unsoundness_kind kind)
    | Recursive -> "Recursive"
    | ReactElementConfigFunArg -> "ReactElementConfigFunArg"
    | Empty_MatchingPropT -> "Empty_MatchingPropT"
    | Empty_TypeDestructorTriggerT loc ->
      Utils_js.spf "Empty_TypeDestructorTriggerT (def: %s)" (Loc.to_string_no_source loc)
    | Empty_SomeUnknownUpper u -> Utils_js.spf "Empty_SomeUnknownUpper (use: %s)" u

  type import_error =
    | Loc_source_none
    | Indeterminate_module_type
    | No_matching_export of string * Loc.t

  type import_error_counts = {
    loc_source_none: int;
    indeterminate_module_type: int;
    no_matching_export: int;
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
      import_error = { loc_source_none = 0; indeterminate_module_type = 0; no_matching_export = 0 };
      serializer_error = 0;
      unsupported_error_kind = 0;
    }

  let combine_import_errors c1 c2 =
    {
      loc_source_none = c1.loc_source_none + c2.loc_source_none;
      indeterminate_module_type = c1.indeterminate_module_type + c2.indeterminate_module_type;
      no_matching_export = c1.no_matching_export + c2.no_matching_export;
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
    | Indeterminate_module_type -> "Indeterminate_module_type"
    | No_matching_export (x, loc) -> spf "No_matching_export %s %s" x (Reason.string_of_loc loc)

  let serialize = function
    | Missing_annotation_or_normalizer_error -> "Missing_annotation_or_normalizer_error"
    | Validation_error e -> "Validation_error " ^ serialize_validation_error e
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
        string_of_row ~indent:4 "Indeterminate module type" c.import_error.indeterminate_module_type;
        string_of_row ~indent:4 "No matching export" c.import_error.no_matching_export;
        string_of_row ~indent:2 "Serializer error" c.serializer_error;
        string_of_row ~indent:2 "Unsupported error kind" c.unsupported_error_kind;
      ]
    in
    String.concat "\n" rows

  let add_import_error c = function
    | Loc_source_none -> { c with loc_source_none = c.loc_source_none + 1 }
    | Indeterminate_module_type ->
      { c with indeterminate_module_type = c.indeterminate_module_type + 1 }
    | No_matching_export _ -> { c with no_matching_export = c.no_matching_export + 1 }

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

module type BASE_STATS = sig
  type t

  val empty : t

  val combine : t -> t -> t

  val serialize : t -> string list

  val report : t -> string list
end

module UnitStats : BASE_STATS with type t = unit = struct
  type t = unit

  let empty = ()

  let combine _ _ = ()

  let serialize _s = []

  let report _s = []
end

module Stats (Extra : BASE_STATS) = struct
  type t = {
    number_of_annotations_added: int;
    total_size_of_annotations: int;
    extra: Extra.t;
  }

  let empty =
    { number_of_annotations_added = 0; total_size_of_annotations = 0; extra = Extra.empty }

  let combine c1 c2 =
    {
      number_of_annotations_added = c1.number_of_annotations_added + c2.number_of_annotations_added;
      total_size_of_annotations = c1.total_size_of_annotations + c2.total_size_of_annotations;
      extra = Extra.combine c1.extra c2.extra;
    }

  let serialize s =
    let open Utils_js in
    let stats =
      [
        spf "annotations_added: %d" s.number_of_annotations_added;
        spf "total_size_of_annotations: %d" s.total_size_of_annotations;
      ]
      @ Extra.serialize s.extra
    in

    spf "(%s)" (String.concat ", " stats)

  let report s =
    let rows =
      [
        string_of_row ~indent:2 "Number of annotations added" s.number_of_annotations_added;
        string_of_row ~indent:2 "Total size of annotations" s.total_size_of_annotations;
      ]
      @ Extra.report s.extra
    in
    String.concat "\n" rows
end

module UntypedAcc (Extra : BASE_STATS) = struct
  type t = {
    changed_set: FilenameSet.t;
    stats: Extra.t;
  }

  let empty = { changed_set = FilenameSet.empty; stats = Extra.empty }

  let update_stats c stats = { c with stats }

  let combine c1 c2 =
    {
      changed_set = FilenameSet.union c1.changed_set c2.changed_set;
      stats = Extra.combine c1.stats c2.stats;
    }

  let debug loc (x : Debug.t) =
    Hh_logger.debug "%s %s" (Debug.serialize x) (Reason.string_of_loc loc)

  let info loc (x : Info.t) = Hh_logger.info "%s %s" (Info.serialize x) (Reason.string_of_loc loc)

  let report _ c =
    let rows =
      [
        print_section "Stats";
        string_of_row ~indent:2 "Files changed" (FilenameSet.cardinal c.changed_set);
      ]
      @ Extra.report c.stats
    in
    String.concat "\n" rows
end

module Acc (Extra : BASE_STATS) = struct
  module Stats = Stats (Extra)

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

  let report _ c =
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
  let suppress_name lint_severities suppress_types preference =
    if LintSettings.get_value Lints.UnclearType lint_severities = Severity.Err then
      if SSet.mem preference suppress_types then
        Some preference
      else
        SSet.choose_opt suppress_types
    else
      None

  let flowfixme_generic_ty ~preference lint_severities suppress_types =
    match suppress_name lint_severities suppress_types preference with
    | Some name ->
      Ty.Generic (Ty_symbol.builtin_symbol (Reason.OrdinaryName name), Ty.TypeAliasKind, None)
    | None -> Ty.Any (Ty.Annotated ALoc.none)

  let flowfixme_ty = flowfixme_generic_ty ~preference:"$FlowFixMe"

  let flowfixme_empty_ty = flowfixme_generic_ty ~preference:"$FlowFixMeEmpty"

  let empty = Ty.Bot Ty.EmptyType

  let flowfixme_ty_default = flowfixme_ty LintSettings.empty_severities SSet.empty

  let flowfixme_ast ~exact_by_default ~lint_severities ~suppress_types =
    flowfixme_ty lint_severities suppress_types
    |> Ty_serializer.type_ { Ty_serializer.exact_by_default }
    |> Base.Result.ok_or_failwith

  let temporary_objectlit_symbol =
    {
      Ty.sym_provenance = Ty.Builtin;
      sym_def_loc = ALoc.none;
      sym_name = Reason.OrdinaryName "$TEMPORARY$object";
      sym_anonymous = false;
    }

  let temporary_arraylit_symbol =
    {
      Ty.sym_provenance = Ty.Builtin;
      sym_def_loc = ALoc.none;
      sym_name = Reason.OrdinaryName "$TEMPORARY$array";
      sym_anonymous = false;
    }
end

(* Validation *)
module Validator = struct
  open Error

  (* Raise an validation_error if there isn't a user facing type that is equivalent to the Ty *)
  class type_validator_visitor loc_of_aloc =
    object
      inherit [_] Ty.endo_ty as super

      method! on_t env t =
        match t with
        (* Recursive types unsupported *)
        | Ty.Any Ty.Recursive ->
          env := Recursive :: !env;
          Ty.explicit_any
        | Ty.Bot (Ty.NoLowerWithUpper (Ty.SomeUnknownUpper u)) ->
          env := Empty_SomeUnknownUpper u :: !env;
          Ty.explicit_any
        | Ty.Bot Ty.EmptyMatchingPropT ->
          env := Empty_MatchingPropT :: !env;
          Ty.explicit_any
        | Ty.Any
            (Ty.Unsound
              ( ( Ty.Constructor | Ty.DummyStatic | Ty.Exports | Ty.FunctionPrototype
                | Ty.InferenceHooks | Ty.InstanceOfRefinement | Ty.Merged | Ty.ResolveSpread
                | Ty.Unchecked | Ty.Unimplemented | Ty.UnresolvedType ) as kind
              )
              ) ->
          env := Any_Unsound kind :: !env;
          Ty.explicit_any
        | Ty.Utility (Ty.ReactElementConfigType (Ty.Fun _)) ->
          env := ReactElementConfigFunArg :: !env;
          Ty.explicit_any
        | Ty.TypeOf (Ty.TSymbol symbol, _)
        | Ty.Generic (symbol, _, _) ->
          let { Ty.sym_anonymous; sym_def_loc; _ } = symbol in
          if sym_anonymous then (
            env := Anonymous (loc_of_aloc sym_def_loc) :: !env;
            Ty.explicit_any
          ) else
            super#on_t env t
        | Ty.Fun f ->
          (* skip validating fun_static to preserve behavior from before fun_static
             was added to fun_t *)
          super#on_t env Ty.(Fun { f with fun_static = Top })
        | _ -> super#on_t env t
    end

  let validate_type_too_big_max = 1000

  let validate_type ~size_limit ~loc_of_aloc t =
    match Ty_utils.size_of_type ~max:size_limit t with
    | None ->
      let max = validate_type_too_big_max in
      let error = TooBig { size_limit; size = Ty_utils.size_of_type ~max t } in
      (t, [error])
    | Some _ ->
      let env = ref [] in
      let t = (new type_validator_visitor loc_of_aloc)#on_t env t in
      (t, !env)
end

(** Add named type parameter to ensure a Flow_ast.Type can be parsed after being
  * pretty printed.
  *
  * This was originally in annotate exports and may not be necissary now if this
  * issue with the pretty-printer/parser has been fixed.
  *)

(** WARNING! Hard-coded fixes ahead!
  *
  * The level of Flow_ast.Type.t nodes: These involve fixes without which the
  * generated types might be unparseable.
  *
  * This fix may be possible to avoid if we fix either the pretty printer
  * or the parser. Not sure which would actually need to be changed.
  *)
class mapper_type_printing_hardcoded_fixes =
  object (this)
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    method private normalize_function ff =
      let open Flow_ast.Type.Function in
      let { params = (loc, { Params.params; rest; this_; comments }); _ } = ff in
      let (normalized_params_rev, _) =
        List.fold_left
          (fun (p, c) param ->
            match param with
            | (loc, { Param.name = None; annot; optional }) ->
              let normalized_param =
                ( loc,
                  {
                    Param.name = Some (Flow_ast_utils.ident_of_source (loc, Printf.sprintf "_%d" c));
                    annot;
                    optional;
                  }
                )
              in
              (normalized_param :: p, c + 1)
            | _ -> (param :: p, c + 1))
          ([], 0)
          params
      in
      let normalized_params = List.rev normalized_params_rev in
      { ff with params = (loc, { Params.params = normalized_params; rest; this_; comments }) }

    method private type_generic_normalize (t : ('loc, 'loc) Flow_ast.Type.t) =
      super#type_
        (match t with
        | (loc_f, Flow_ast.Type.Function ff) ->
          let nf = this#normalize_function ff in
          (loc_f, Flow_ast.Type.Function nf)
        | _ -> t)

    method! type_args (targs : ('loc, 'loc) Flow_ast.Type.TypeArgs.t) =
      let open Flow_ast.Type.TypeArgs in
      let (loc, { arguments = ts; comments }) = targs in
      let ts' = Base.List.map ~f:this#type_generic_normalize ts in
      if ts' == ts then
        targs
      else
        (loc, { arguments = ts'; comments })
  end

let patch_up_type_ast = (new mapper_type_printing_hardcoded_fixes)#type_

(* Apply stylistic changes to react types *)
class patch_up_react_mapper ?(imports_react = false) () =
  object (this)
    inherit [_] Ty.endo_ty as super

    method! on_t loc t =
      match t with
      (* If 'react' is not imported, then we treat the symbol as Remote, so that
       * it is imported with the same mechanism we import other Remote symbols.
       * Otherwise, we refer to these names as 'React.NAME'. *)
      | Ty.Generic
          ( ( {
                Ty.sym_name =
                  Reason.OrdinaryName
                    ( ( "AbstractComponent" | "ChildrenArray" | "ComponentType" | "Config"
                      | "Context" | "Element" | "MixedElement" | "ElementConfig" | "ElementProps"
                      | "ElementRef" | "ElementType" | "Key" | "Node" | "Portal" | "Ref" ) as name
                    );
                sym_provenance = Ty_symbol.Library { Ty_symbol.imported_as = None };
                sym_def_loc;
                _;
              } as symbol
            ),
            kind,
            args_opt
          )
        when is_react_loc sym_def_loc ->
        let args_opt = Flow_ast_mapper.map_opt (ListUtils.ident_map (this#on_t loc)) args_opt in
        let symbol =
          if imports_react then
            { symbol with Ty.sym_name = Reason.OrdinaryName ("React." ^ name) }
          else
            { symbol with Ty.sym_provenance = Ty.Remote { Ty.imported_as = None } }
        in
        Ty.Generic (symbol, kind, args_opt)
      | _ -> super#on_t loc t

    method! on_prop loc prop =
      let prop =
        match prop with
        | Ty.NamedProp { name; prop = named_prop; inherited; source; def_locs }
          when Reason.is_internal_name name ->
          Hh_logger.warn
            "ShadowProp %s at %s"
            (Reason.display_string_of_name name)
            (Reason.string_of_loc loc);
          (* Shadow props appear as regular props *)
          let name = Reason.OrdinaryName (Reason.uninternal_name name) in
          Ty.NamedProp { name; prop = named_prop; inherited; source; def_locs }
        | prop -> prop
      in
      super#on_prop loc prop
  end

let reverse_append_all : 'a list list -> 'a list = List.fold_left List.rev_append []

type partition_acc = {
  bools: Ty.t list;
  nums: Ty.t list;
  strings: Ty.t list;
  others: Ty.t list;
}

class stylize_ty_mapper ?(imports_react = false) () =
  object
    inherit patch_up_react_mapper ~imports_react () as super

    (* remove literals when the base type is in the union, and simplify true | false to bool *)
    (* These simplifications should always be sound *)
    method! on_Union loc t from_bounds _ _ _ =
      Ty.(
        let filter_union (a : partition_acc) t =
          match (t, a) with
          (* If element of a base type and the base type is already present in the union
           * ignore the element *)
          | ((Bool None | BoolLit _), { bools = [Bool None]; _ })
          | ((Num None | NumLit _), { nums = [Num None]; _ })
          | ((Str None | StrLit _), { strings = [Str None]; _ }) ->
            a
          (* Otherwise, if we see the base element automatically discard all other elements *)
          | (Bool None, _) -> { a with bools = [t] }
          | (Num None, _) -> { a with nums = [t] }
          | (Str None, _) -> { a with strings = [t] }
          (* Otherwise, if it is bool check to see if we have enumerated both element *)
          | (BoolLit true, { bools = [BoolLit false]; _ })
          | (BoolLit false, { bools = [BoolLit true]; _ }) ->
            { a with bools = [Bool None] }
          (* Otherwise, add literal types to the union *)
          | (BoolLit _, { bools; _ }) -> { a with bools = t :: bools }
          | (NumLit _, { nums; _ }) -> { a with nums = t :: nums }
          | (StrLit _, { strings; _ }) -> { a with strings = t :: strings }
          (* Note, any temporary base types get passed through with others *)
          | (t, { others; _ }) -> { a with others = t :: others }
        in
        let empty = { bools = []; nums = []; strings = []; others = [] } in
        let { bools; nums; strings; others } = Nel.fold_left filter_union empty (bk_union t) in
        match reverse_append_all [others; strings; nums; bools] with
        | [] -> failwith "Impossible! this only removes elements when others are added/present"
        | [t] -> super#on_t loc t
        | t1 :: t2 :: ts ->
          super#on_Union loc (Union (from_bounds, t1, t2, ts)) from_bounds t1 t2 ts
      )
  end

(* Returns true if the location given a zero width location. *)
let is_point loc = Loc.(loc.start = loc._end)

module PreserveLiterals = struct
  type mode =
    | Always
    | Never
    | Auto

  (* A string literal that contains alphabetic characters of underscores is likely
   * to be a tag *)
  let tag_like_regex = Str.regexp "^[a-zA-Z0-9-_]+$"

  let enforce ~mode t =
    let enforce_string s =
      match mode with
      | Always -> t
      | Never -> Ty.Str None
      | Auto ->
        if Str.string_match tag_like_regex s 0 then
          t
        else
          Ty.Str None
    in
    let enforce_number =
      match mode with
      | Always -> t
      | Never
      | Auto ->
        Ty.Num None
    in
    let enforce_bool =
      match mode with
      | Always -> t
      | Never
      | Auto ->
        Ty.Bool None
    in
    match t with
    | Ty.Str (Some s) ->
      (* TODO consider handling internal names explicitly *)
      enforce_string (Reason.display_string_of_name s)
    | Ty.Num (Some _) -> enforce_number
    | Ty.Bool (Some _) -> enforce_bool
    | _ -> t
end

(* Given a GraphQL file foo.graphql.js exporting a type
 *
 *   export type Foo = {|
 *     +f?: {|
 *       +g: {|
 *         +h: ?$ReadOnlyArray<{|
 *           +i: {||}
 *         |}>
 *       |};
 *     |}
 *   |};
 *
 * [extract_graphql_fragment cx file_sig typed_ast tgt_aloc] returns the type of
 * the AST node at location [tgt_aloc] expressed in terms of an exported type alias:
 *
 *   $NonMaybeType<Foo?.["f"]?.["g"]?.["h"]?.[0]?.["i"]>
 *
 * Note that the solution falls back to optional index chaining as soon as the first
 * optional type or property is encountered. In that case, we prefix the resulting
 * type with `$NonMaybeType`.
 *)
module GraphQL : sig
  val extract_graphql_fragment :
    Context.t -> File_sig.t -> (ALoc.t, ALoc.t * Type.t) Ast.Program.t -> ALoc.t -> Ty.t option
end = struct
  let reader = State_reader.create ()

  let loc_of_aloc = Parsing_heaps.Reader.loc_of_aloc ~reader

  let abstract_reader = Abstract_state_reader.State_reader reader

  let rec visit_object_property_type defs tgt ~opt_chain ty p =
    let open Ast.Type.Object.Property in
    let (_, { key; value; optional; static = _; proto = _; _method = _; variance = _; comments = _ })
        =
      p
    in
    let open Ast.Expression.Object.Property in
    match key with
    | Identifier (_, { Ast.Identifier.name; _ })
    | StringLiteral (_, { Ast.StringLiteral.value = name; _ }) -> begin
      match value with
      | Init t ->
        let optional = opt_chain || optional in
        let ty' =
          Ty.IndexedAccess { _object = ty; index = Ty.StrLit (Reason.OrdinaryName name); optional }
        in
        visit_type defs tgt ~opt_chain:optional ty' t
      | Get _
      | Set _ ->
        None
    end
    | NumberLiteral _
    | BigIntLiteral _
    | PrivateName _
    | Computed _ ->
      None

  and visit_object_type defs tgt ~opt_chain ty ot =
    let open Ast.Type.Object in
    let { properties; exact = _; inexact = _; comments = _ } = ot in
    Base.List.find_map properties ~f:(fun p ->
        match p with
        | Property p -> visit_object_property_type defs tgt ~opt_chain ty p
        | SpreadProperty _
        | Indexer _
        | InternalSlot _
        | MappedType _
        | CallProperty _ ->
          None
    )

  and visit_readonlyarray defs tgt ~opt_chain ty t' =
    let ty' = Ty.IndexedAccess { _object = ty; index = Ty.NumLit "0"; optional = opt_chain } in
    visit_type defs tgt ~opt_chain ty' t'

  and visit_type defs tgt ~opt_chain ty t =
    let open Ast.Type in
    match t with
    | (oloc, Object ot) ->
      if tgt = oloc then
        Some (ty, opt_chain)
      else
        visit_object_type defs tgt ~opt_chain ty ot
    | (_, Nullable { Nullable.argument = (oloc, Object ot); _ }) ->
      if tgt = oloc then
        Some (ty, true)
      else
        visit_object_type defs tgt ~opt_chain:true ty ot
    | ( _,
        Generic
          {
            Generic.id =
              Generic.Identifier.Unqualified (_, { Ast.Identifier.name = "$ReadOnlyArray"; _ });
            targs = Some (_, { Ast.Type.TypeArgs.arguments = [t']; _ });
            _;
          }
      ) ->
      visit_readonlyarray defs tgt ~opt_chain ty t'
    | ( _,
        Nullable
          {
            Nullable.argument =
              ( _,
                Generic
                  {
                    Generic.id =
                      Generic.Identifier.Unqualified
                        (_, { Ast.Identifier.name = "$ReadOnlyArray"; _ });
                    targs = Some (_, { Ast.Type.TypeArgs.arguments = [t']; _ });
                    _;
                  }
              );
            _;
          }
      ) ->
      visit_readonlyarray defs tgt ~opt_chain:true ty t'
    | _ -> None

  let visit_type_alias defs tgt type_alias =
    let open Ast.Statement.TypeAlias in
    let open Base.Option.Let_syntax in
    match type_alias with
    | { id; tparams = None; right; comments = _ } ->
      let (id_loc, { Ast.Identifier.name; _ }) = id in
      let (name, sym_provenance) =
        match Loc_collections.LocMap.find_opt id_loc defs with
        | Some (_, local_name, _) -> (local_name, Ty_symbol.Local)
        | None -> (name, Ty_symbol.Remote { Ty_symbol.imported_as = None })
      in
      let symbol =
        {
          Ty_symbol.sym_provenance;
          sym_def_loc = ALoc.of_loc id_loc;
          sym_name = Reason.OrdinaryName name;
          sym_anonymous = false;
        }
      in
      let ty = Ty.Generic (symbol, Ty.TypeAliasKind, None) in
      let%map (ty, opt_chain) = visit_type defs tgt ~opt_chain:false ty right in
      if opt_chain then
        Ty.Utility (Ty.NonMaybeType ty)
      else
        ty
    | _ -> None

  let visit_declaration defs tgt decl =
    let open Ast.Statement in
    match decl with
    | (_loc, TypeAlias type_alias) -> visit_type_alias defs tgt type_alias
    | _ -> None

  let visit_statement defs tgt stmt =
    let open Ast.Statement in
    match stmt with
    | ( _loc,
        ExportNamedDeclaration
          {
            ExportNamedDeclaration.export_kind = ExportType;
            source = _;
            specifiers = _;
            declaration = Some decl;
            _;
          }
      ) ->
      visit_declaration defs tgt decl
    | _ -> None

  let visit_program defs tgt prog =
    let open Ast.Program in
    let (_, { statements; _ }) = prog in
    Base.List.find_map ~f:(visit_statement defs tgt) statements

  let get_imported_ident cx (local_name, loc, import_mode, { Type.TypeScheme.type_; _ }) =
    let t =
      match Ty_normalizer.Lookahead.peek cx type_ with
      | Ty_normalizer.Lookahead.LowerBounds [t] -> t
      | Ty_normalizer.Lookahead.LowerBounds _
      | Ty_normalizer.Lookahead.Recursive ->
        type_
    in
    (loc_of_aloc (TypeUtil.def_loc_of_t t), (loc, local_name, import_mode))

  let extract_graphql_fragment cx file_sig typed_ast tgt_aloc =
    let graphql_file = Base.Option.value_exn (ALoc.source tgt_aloc) in
    let graphql_ast =
      Parsing_heaps.Reader_dispatcher.get_ast_unsafe ~reader:abstract_reader graphql_file
    in
    (* Collect information about imports in currect file to accurately compute
     * wether the base GraphQL type is imported in the current file. *)
    let defs =
      Ty_normalizer_imports.extract_schemes cx file_sig (Some typed_ast)
      |> List.map (get_imported_ident cx)
      |> Loc_collections.LocMap.of_list
    in
    let tgt_loc = loc_of_aloc tgt_aloc in
    let r = visit_program defs tgt_loc graphql_ast in
    if Base.Option.is_none r then
      Hh_logger.info "Failed to extract GraphQL type fragment %s" (Reason.string_of_loc tgt_loc);
    r
end

class type_normalization_hardcoded_fixes_mapper
  ~cx
  ~file_sig
  ~typed_ast
  ~lint_severities
  ~suppress_types
  ~imports_react
  ~preserve_literals
  ~generalize_maybe
  ~generalize_react_mixed_element
  ~add_warning =
  let metadata = Context.metadata cx in
  object (this)
    inherit stylize_ty_mapper ~imports_react () as super

    val sanitized_any = Builtins.flowfixme_ty lint_severities suppress_types

    method! on_Union env _ from_bounds ty1 ty2 tys =
      (* The following moves the `any` component of a union to the beginning of the
       * union. This is a heuristic that helps union resolution later on. *)
      let (ty1, ty2, tys) =
        match (ty1, ty2) with
        | (Ty.Any _, _) -> (sanitized_any, ty2, tys)
        | (_, Ty.Any _) -> (sanitized_any, ty1, tys)
        | _ ->
          if List.exists Ty.is_dynamic tys then
            let tys' = List.filter Utils_js.(Ty.is_dynamic %> not) tys in
            if tys == tys' then
              (ty1, ty2, tys)
            else
              (sanitized_any, ty1, ty2 :: tys')
          else
            (ty1, ty2, tys)
      in

      (*
       * If there are named type aliases in a union generated by a codemod (i.e. from_bounds
       * is true), they are likely the best types to use. The other union branches
       * with literal types are usually noise.
       *)
      let ts = ty1 :: ty2 :: tys in
      let has_type_aliases =
        List.exists
          (function
            | Ty.Generic (_, Ty.TypeAliasKind, _) -> true
            | _ -> false)
          ts
      in
      let ts =
        if has_type_aliases && from_bounds then
          List.filter
            (function
              | Ty.Obj { Ty.obj_literal = Some true; _ }
              | Ty.Num (Some _)
              | Ty.Str (Some _)
              | Ty.Bool (Some _) ->
                false
              | _ -> true)
            ts
        else
          ts
      in

      (* At this point we can recurse on the overall union, and then destruct it again
         for further analysis. We need to do this because of other transformations that
         won't fire correctly--e.g. we need to have not simplified literals for the
         `has_type_aliases` step above, but we must have simplified in order to see a
         unified representation for Fbt as below. *)
      match ts with
      | [] -> super#on_t env (Ty.mk_union ~from_bounds (ty1, ty2 :: tys))
      | [t] -> this#on_t env t
      | ts ->
        let ts = Base.List.map ~f:(this#on_t env) ts in

        (* The Fbt type contains string *)
        let has_fbt =
          List.exists
            (function
              | Ty.Generic
                  ( {
                      Ty.sym_name = Reason.OrdinaryName "Fbt";
                      sym_provenance = Ty_symbol.Library _;
                      _;
                    },
                    _,
                    None
                  ) ->
                true
              | _ -> false)
            ts
        in
        let ts =
          if has_fbt && from_bounds then
            List.filter
              (function
                | Ty.Str _ -> false
                | _ -> true)
              ts
          else
            ts
        in

        let ts =
          if generalize_maybe && from_bounds && List.length ts > 1 then
            if List.mem Ty.Null ts && not (List.mem Ty.Void ts) then
              Ty.Void :: ts
            else if List.mem Ty.Void ts && not (List.mem Ty.Null ts) then
              Ty.Null :: ts
            else
              ts
          else
            ts
        in

        (* Array<empty> or Array<any>, commonly from [], is never a useful type.
           We should remove it from the union. *)
        let ts =
          let ts_without_array_empty =
            List.filter
              (function
                | Ty.Arr { Ty.arr_elt_t = Ty.Bot _; _ } -> false
                | Ty.Arr { Ty.arr_elt_t = Ty.Any _; _ } -> false
                | _ -> true)
              ts
          in
          if Base.List.is_empty ts_without_array_empty then
            ts
          else
            ts_without_array_empty
        in

        (* React.MixedElement | React.Element<'div'> becomes just React.MixedElement *)
        let ts =
          let react_element_def_loc =
            List.find_map
              (function
                | Ty.Generic
                    ( {
                        Ty.sym_name =
                          Reason.OrdinaryName
                            ("Element" | "MixedElement" | "React.Element" | "React.MixedElement");
                        sym_provenance = Ty_symbol.Library _;
                        sym_def_loc;
                        _;
                      },
                      _,
                      _
                    )
                  when is_react_loc sym_def_loc ->
                  Some sym_def_loc
                | _ -> None)
              ts
          in
          match react_element_def_loc with
          | Some react_element_def_loc when from_bounds && generalize_react_mixed_element ->
            let ts =
              List.filter
                (function
                  | Ty.Generic
                      ( {
                          Ty.sym_name =
                            Reason.OrdinaryName
                              ("Element" | "MixedElement" | "React.Element" | "React.MixedElement");
                          sym_provenance = Ty_symbol.Library _;
                          sym_def_loc;
                          _;
                        },
                        _,
                        Some _
                      ) ->
                    not (is_react_loc sym_def_loc)
                  | Ty.Generic
                      ( {
                          Ty.sym_name = Reason.OrdinaryName "Fbt";
                          sym_provenance = Ty_symbol.Library _;
                          _;
                        },
                        _,
                        None
                      )
                    when metadata.Context.facebook_fbt <> None ->
                    false
                  | _ -> true)
                ts
            in
            Ty.Generic
              ( {
                  Ty.sym_name = Reason.OrdinaryName "React.MixedElement";
                  sym_provenance = Ty_symbol.Library { Ty_symbol.imported_as = None };
                  sym_def_loc = react_element_def_loc;
                  sym_anonymous = false;
                },
                Ty.TypeAliasKind,
                None
              )
            :: ts
          | _ -> ts
        in

        (* React.Node | React.Element<'div'> becomes just React.Node *)
        let has_react_node =
          List.exists
            (function
              | Ty.Generic
                  ( {
                      Ty.sym_name = Reason.OrdinaryName ("Node" | "React.Node");
                      sym_provenance = Ty_symbol.Library _;
                      sym_def_loc;
                      _;
                    },
                    _,
                    _
                  ) ->
                is_react_loc sym_def_loc
              | _ -> false)
            ts
        in
        let ts =
          if has_react_node && from_bounds then
            List.filter
              (function
                | Ty.Generic
                    ( {
                        Ty.sym_name =
                          Reason.OrdinaryName
                            ("Element" | "MixedElement" | "React.Element" | "React.MixedElement");
                        sym_provenance = Ty_symbol.Library _;
                        sym_def_loc;
                        _;
                      },
                      _,
                      Some _
                    ) ->
                  not (is_react_loc sym_def_loc)
                | Ty.Generic
                    ( {
                        Ty.sym_name = Reason.OrdinaryName "Fbt";
                        sym_provenance = Ty_symbol.Library _;
                        _;
                      },
                      _,
                      None
                    )
                  when metadata.Context.facebook_fbt <> None ->
                  false
                | _ -> true)
              ts
          else
            ts
        in

        (match ts with
        | [] -> Ty.mk_union ~from_bounds (ty1, ty2 :: tys)
        | [t] -> t
        | t :: ts -> Ty.mk_union ~from_bounds (t, ts))

    method! on_t env t =
      let loc = env in
      match t with
      (* `empty` with no upper bounds becomes `any`. All other allowed occurrence
       * `empty` remain as is. *)
      | Ty.Bot (Ty.NoLowerWithUpper Ty.NoUpper) ->
        add_warning loc Warning.Empty_NoUpper;
        Builtins.flowfixme_empty_ty lint_severities suppress_types
      | Ty.Bot Ty.EmptyType ->
        (* `empty` annotations remain *)
        t
      | Ty.Bot (Ty.NoLowerWithUpper (Ty.SomeKnownUpper ub)) ->
        (* empty with `UseT ub` upper bounds becomes `ub` *)
        add_warning loc Warning.Empty_SomeKnownUpper;
        this#on_t env ub
      (* Heuristic: These are rarely useful as full precision literal types *)
      | Ty.Num _
      | Ty.Bool _
      | Ty.Str _ ->
        PreserveLiterals.enforce ~mode:preserve_literals t
      (* E.g. React$Element<'div'> will become React.Element<'div'> *)
      | Ty.Generic
          ( ( {
                Ty.sym_name = Reason.OrdinaryName "React$Element";
                sym_provenance = Ty_symbol.Library _;
                sym_def_loc;
                _;
              } as symbol
            ),
            kind,
            Some [(Ty.Str _ | Ty.StrLit _)]
          )
        when is_react_loc sym_def_loc && imports_react ->
        let name =
          if imports_react then
            "React.MixedElement"
          else
            "React$MixedElement"
        in
        let symbol = { symbol with Ty.sym_name = Reason.OrdinaryName name } in
        this#on_t env Ty.(Generic (symbol, kind, None))
      (* E.g. React$Element<typeof A> will become React.MixedElement or React.Node.
         The reason for this conversion is that it's a common idiom to keep SomeComponentClass to
         a local scope (e.g. that of a function) which would make the annotation ill-formed. *)
      | Ty.Generic
          ( ( {
                Ty.sym_name = Reason.OrdinaryName "React$Element";
                sym_provenance = Ty_symbol.Library _;
                sym_def_loc;
                _;
              } as symbol
            ),
            kind,
            Some _
          )
        when is_react_loc sym_def_loc ->
        let name =
          if generalize_react_mixed_element then
            if imports_react then
              "React.MixedElement"
            else
              "React$MixedElement"
          else if imports_react then
            "React.Node"
          else
            "React$Node"
        in
        let symbol = { symbol with Ty.sym_name = Reason.OrdinaryName name } in
        this#on_t env Ty.(Generic (symbol, kind, None))
      | Ty.Generic
          ( ( {
                Ty.sym_name =
                  Reason.OrdinaryName
                    ("FbtElement" | "FbtResultBase" | "$FbtResultBase" | "FbtString");
                sym_provenance = Ty_symbol.Library _;
                _;
              } as symbol
            ),
            _,
            None
          )
        when metadata.Context.facebook_fbt = Some "FbtElement" ->
        let symbol = { symbol with Ty.sym_name = Reason.OrdinaryName "Fbt" } in
        Ty.(Generic (symbol, Ty.TypeAliasKind, None))
      (*
       * Minimal form of evaluating utility types:
       *
       * $Call<<K>(_0: K) => K, T, ...> --> T
       *)
      | Ty.(
          Utility
            (Call
              ( Fun
                  {
                    fun_type_params =
                      Some
                        [{ tp_name = p; tp_bound = None; tp_polarity = Neutral; tp_default = None }];
                    fun_params = [(_, Bound (_, b), { prm_optional = false })];
                    fun_rest_param = None;
                    fun_return = ReturnType (Bound (_, r));
                    fun_static = _;
                    fun_hook = _;
                  },
                t :: _
              )
              ))
        when p = b && b = r ->
        this#on_t env t
      | Ty.Obj { Ty.obj_def_loc = Some aloc; _ } ->
        let remote_file = Base.Option.value_exn (ALoc.source aloc) in
        if String.ends_with (File_key.to_string remote_file) ~suffix:"graphql.js" then
          match GraphQL.extract_graphql_fragment cx file_sig typed_ast aloc with
          | Some t -> t
          | None -> super#on_t env t
        else
          super#on_t env t
      (* All any's that have reached this point will be serialized. By making them
       * all of the same kind we enable type simplification (which depends on
       * structural equality). *)
      | Ty.Any _ -> sanitized_any
      | _ -> super#on_t env t
  end

module MakeHardcodedFixes (Extra : BASE_STATS) = struct
  module Acc = Acc (Extra)

  (* Converts types like 'Array<T> | Array<S> | R' to '$ReadOnlyArray<T | S> | R'
   * In certain kinds of codemods this has shown to cause fewer [ambiguous-speculation]
   * errros. *)
  let array_simplification t =
    let open Ty in
    let arr_elts ts =
      List.fold_left
        (fun (arr_acc, other_acc) t ->
          match t with
          | Arr { arr_elt_t; _ } -> (arr_elt_t :: arr_acc, other_acc)
          | _ -> (arr_acc, t :: other_acc))
        ([], [])
        ts
    in
    let members = bk_union t |> Nel.to_list in
    let (arr_members, other_members) = arr_elts members in
    match arr_members with
    | []
    | [_] ->
      t
    | t :: ts ->
      let arr =
        Arr
          {
            arr_elt_t = mk_union ~from_bounds:true (t, ts);
            arr_literal = None;
            arr_readonly = true;
          }
      in
      mk_union ~from_bounds:true (arr, other_members)

  let run
      ~cx
      ~file_sig
      ~typed_ast
      ~preserve_literals
      ~generalize_maybe
      ~generalize_react_mixed_element
      ~merge_arrays
      ~lint_severities
      ~suppress_types
      ~imports_react
      acc
      loc
      t =
    let acc_ref = ref acc in
    let add_warning loc warning = acc_ref := Acc.warn !acc_ref loc warning in
    let mapper =
      new type_normalization_hardcoded_fixes_mapper
        ~cx
        ~file_sig
        ~typed_ast
        ~lint_severities
        ~suppress_types
        ~imports_react
        ~preserve_literals
        ~generalize_maybe
        ~generalize_react_mixed_element
        ~add_warning
    in
    let t' = mapper#on_t loc t in
    let t' =
      if merge_arrays then
        array_simplification t'
      else
        t'
    in
    let t'' =
      if t == t' then
        t
      else
        Ty_utils.simplify_type ~merge_kinds:false t'
    in
    (!acc_ref, t'')
end
