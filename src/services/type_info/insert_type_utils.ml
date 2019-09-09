(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

let warn_shadow_prop ?(strip_root = None) name loc =
  Hh_logger.warn "ShadowProp %s at %s" name (Reason.string_of_loc ?strip_root loc)

exception Fatal of validation_error

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
      | Ty.Generic (symbol, _, _)
      | Ty.ClassDecl (symbol, _)
      | Ty.InterfaceDecl (symbol, _)
      | Ty.Module (Some symbol, _) ->
        let { Ty.anonymous; def_loc; _ } = symbol in
        if anonymous then (
          env := Anonymous (ALoc.to_loc_exn def_loc) :: !env;
          Ty.explicit_any
        ) else
          super#on_t env t
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
      Flow_ast.Type.Function.(
        let { params = (loc, { Params.params; rest }); _ } = ff in
        let (normalized_params_rev, _) =
          List.fold_left
            (fun (p, c) param ->
              match param with
              | (loc, { Param.name = None; annot; optional }) ->
                let normalized_param =
                  ( loc,
                    {
                      Param.name =
                        Some (Flow_ast_utils.ident_of_source (loc, Printf.sprintf "_%d" c));
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
        { ff with params = (loc, { Params.params = normalized_params; rest }) })

    method private type_generic_normalize (t : ('loc, 'loc) Flow_ast.Type.t) =
      super#type_
        (match t with
        | (loc_f, Flow_ast.Type.Function ff) ->
          let nf = this#normalize_function ff in
          (loc_f, Flow_ast.Type.Function nf)
        | _ -> t)

    method! type_parameter_instantiation (pi : ('loc, 'loc) Flow_ast.Type.ParameterInstantiation.t)
        =
      let (loc, targs) = pi in
      let targs' = Core_list.map ~f:this#type_generic_normalize targs in
      if targs' == targs then
        pi
      else
        (loc, targs')
  end

let patch_up_type_ast = (new mapper_type_printing_hardcoded_fixes)#type_

(* returns true if a file_key is a libfile of name react.js *)
let is_react_file_key = function
  | File_key.LibFile x -> Filename.basename x = "react.js"
  | _ -> false

(* returns true if a location has a source of react.js *)
let is_react_loc loc =
  match ALoc.source loc with
  | Some f -> is_react_file_key f
  | _ -> false

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
                Ty.name =
                  ( "AbstractComponent" | "ChildrenArray" | "ComponentType" | "Config" | "Context"
                  | "Element" | "ElementConfig" | "ElementProps" | "ElementRef" | "ElementType"
                  | "Key" | "Node" | "Portal" | "Ref" | "StatelessFunctionalComponent" ) as name;
                provenance = Ty_symbol.Library;
                def_loc;
                _;
              } as symbol ),
            kind,
            args_opt )
        when is_react_loc def_loc ->
        let args_opt = Flow_ast_mapper.map_opt (ListUtils.ident_map (this#on_t loc)) args_opt in
        let symbol =
          if imports_react then
            { symbol with Ty.name = "React." ^ name }
          else
            { symbol with Ty.provenance = Ty.Remote { Ty.imported_as = None } }
        in
        Ty.Generic (symbol, kind, args_opt)
      | _ -> super#on_t loc t

    method! on_prop loc prop =
      let prop =
        match prop with
        | Ty.NamedProp (name, named_prop) when Reason.is_internal_name name ->
          warn_shadow_prop name loc;

          (* Shadow props appear as regular props *)
          let name = String.sub name 1 (String.length name - 1) in
          Ty.NamedProp (name, named_prop)
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

let temporary_objectlit_symbol =
  {
    Ty.provenance = Ty.Builtin;
    def_loc = ALoc.none;
    name = "$TEMPORARY$object";
    anonymous = false;
  }

let temporary_arraylit_symbol =
  { Ty.provenance = Ty.Builtin; def_loc = ALoc.none; name = "$TEMPORARY$array"; anonymous = false }

module Builtins = struct
  let flowfixme = Ty.Generic (Ty_symbol.builtin_symbol "$FlowFixMe", Ty.TypeAliasKind, None)

  let flowfixme_empty =
    Ty.Generic (Ty_symbol.builtin_symbol "$FlowFixMeEmpty", Ty.TypeAliasKind, None)

  let empty = Ty.Bot Ty.EmptyType
end
