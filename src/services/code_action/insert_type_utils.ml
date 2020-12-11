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

    let compare = Stdlib.compare
  end)

  include M
end

module SymbolSet = Set.Make (struct
  type t = Ty_symbol.symbol

  let compare = Stdlib.compare
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
    | Parsing_heaps_get_ast_error
    | Indeterminate_module_type
    | No_matching_export of string * ALoc.t

  type import_error_counts = {
    loc_source_none: int;
    parsing_heaps_get_ast_error: int;
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
      import_error =
        {
          loc_source_none = 0;
          parsing_heaps_get_ast_error = 0;
          indeterminate_module_type = 0;
          no_matching_export = 0;
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
        string_of_row
          ~indent:4
          "Parsing heaps get ast error"
          c.import_error.parsing_heaps_get_ast_error;
        string_of_row ~indent:4 "Indeterminate module type" c.import_error.indeterminate_module_type;
        string_of_row ~indent:4 "No matching export" c.import_error.no_matching_export;
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
    | Some name -> Ty.Generic (Ty_symbol.builtin_symbol name, Ty.TypeAliasKind, None)
    | None -> Ty.Any Ty.Annotated

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
      sym_name = "$TEMPORARY$object";
      sym_anonymous = false;
    }

  let temporary_arraylit_symbol =
    {
      Ty.sym_provenance = Ty.Builtin;
      sym_def_loc = ALoc.none;
      sym_name = "$TEMPORARY$array";
      sym_anonymous = false;
    }
end

(* Validation *)
module Validator = struct
  open Error

  (* Raise an validation_error if there isn't a user facing type that is equivalent to the Ty *)
  class type_validator_visitor =
    object
      inherit [_] Ty.endo_ty as super

      method! on_t env t =
        match t with
        (* Recursive types unsupported *)
        | Ty.Mu _
        | Ty.TVar _ ->
          env := Recursive :: !env;
          Ty.explicit_any
        | Ty.Bot (Ty.NoLowerWithUpper (Ty.SomeUnknownUpper u)) ->
          env := Empty_SomeUnknownUpper u :: !env;
          Ty.explicit_any
        | Ty.Bot Ty.EmptyMatchingPropT ->
          env := Empty_MatchingPropT :: !env;
          Ty.explicit_any
        | Ty.Bot (Ty.EmptyTypeDestructorTriggerT loc) ->
          env := Empty_TypeDestructorTriggerT (ALoc.to_loc_exn loc) :: !env;
          Ty.explicit_any
        | Ty.Any
            (Ty.Unsound
              ( ( Ty.Constructor | Ty.DummyStatic | Ty.Existential | Ty.Exports
                | Ty.FunctionPrototype | Ty.InferenceHooks | Ty.InstanceOfRefinement | Ty.Merged
                | Ty.ResolveSpread | Ty.Unchecked | Ty.Unimplemented | Ty.UnresolvedType
                | Ty.WeakContext ) as kind )) ->
          env := Any_Unsound kind :: !env;
          Ty.explicit_any
        | Ty.Utility (Ty.ReactElementConfigType (Ty.Fun _)) ->
          env := ReactElementConfigFunArg :: !env;
          Ty.explicit_any
        | Ty.TypeOf (Ty.TSymbol symbol)
        | Ty.Generic (symbol, _, _) ->
          let { Ty.sym_anonymous; sym_def_loc; _ } = symbol in
          if sym_anonymous then (
            env := Anonymous (ALoc.to_loc_exn sym_def_loc) :: !env;
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

  let validate_type ~size_limit t =
    match Ty_utils.size_of_type ~max:size_limit t with
    | None ->
      let max = validate_type_too_big_max in
      let error = TooBig { size_limit; size = Ty_utils.size_of_type ~max t } in
      (t, [error])
    | Some _ ->
      let env = ref [] in
      let t = (new type_validator_visitor)#on_t env t in
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
                  } )
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
                  ( "AbstractComponent" | "ChildrenArray" | "ComponentType" | "Config" | "Context"
                  | "Element" | "ElementConfig" | "ElementProps" | "ElementRef" | "ElementType"
                  | "Key" | "Node" | "Portal" | "Ref" | "StatelessFunctionalComponent" ) as name;
                sym_provenance = Ty_symbol.Library { Ty_symbol.imported_as = None };
                sym_def_loc;
                _;
              } as symbol ),
            kind,
            args_opt )
        when is_react_loc sym_def_loc ->
        let args_opt = Flow_ast_mapper.map_opt (ListUtils.ident_map (this#on_t loc)) args_opt in
        let symbol =
          if imports_react then
            { symbol with Ty.sym_name = "React." ^ name }
          else
            { symbol with Ty.sym_provenance = Ty.Remote { Ty.imported_as = None } }
        in
        Ty.Generic (symbol, kind, args_opt)
      | _ -> super#on_t loc t

    method! on_prop loc prop =
      let prop =
        match prop with
        | Ty.NamedProp { name; prop = named_prop; from_proto; def_loc }
          when Reason.is_internal_name name ->
          Hh_logger.warn "ShadowProp %s at %s" name (Reason.string_of_loc loc);
          (* Shadow props appear as regular props *)
          let name = String.sub name 1 (String.length name - 1) in
          Ty.NamedProp { name; prop = named_prop; from_proto; def_loc }
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
    method! on_Union loc t _ _ _ =
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
        | t1 :: t2 :: ts -> super#on_Union loc (Union (t1, t2, ts)) t1 t2 ts)
  end

(* Returns true if the location given a zero width location. *)
let is_point loc = Loc.(loc.start = loc._end)
