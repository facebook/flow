(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type validation_error =
  | TooBig
  | Anonymous of Loc.t
  | Recursive
  | ReactElementConfigFunArg
  | Empty_MatchingPropT
  | Empty_TypeDestructorTriggerT of Loc.t
  | Empty_SomeUnknownUpper of string

let serialize_validation_error = function
  | TooBig -> "TooBig"
  | Anonymous loc -> Utils_js.spf "Anonymous (def: %s)" (Loc.to_string_no_source loc)
  | Recursive -> "Recursive"
  | ReactElementConfigFunArg -> "ReactElementConfigFunArg"
  | Empty_MatchingPropT -> "Empty_MatchingPropT"
  | Empty_TypeDestructorTriggerT loc ->
    Utils_js.spf "Empty_TypeDestructorTriggerT (def: %s)" (Loc.to_string_no_source loc)
  | Empty_SomeUnknownUpper u ->
    Utils_js.spf "Empty_SomeUnknownUpper (use: %s)" u

let warn_shadow_prop ?(strip_root=None) name loc =
    Hh_logger.warn "ShadowProp %s at %s" name (Reason.string_of_loc ?strip_root loc)

exception Fatal of validation_error
(* Raise an validation_error if there isn't a user facing type that is equivalent to the Ty *)
class type_validator_visitor = object(_)
  inherit [_] Ty.iter_ty as super
  method! on_t () t =
    match t with
    | Ty.Mu _ -> raise (Fatal Recursive)

    (* We cannot serialize the following Bot-like types *)
    | Ty.Bot (Ty.NoLowerWithUpper (Ty.SomeUnknownUpper u)) ->
      raise (Fatal (Empty_SomeUnknownUpper u))

    | Ty.Bot Ty.EmptyMatchingPropT ->
      raise (Fatal Empty_MatchingPropT)

    | Ty.Bot (Ty.EmptyTypeDestructorTriggerT loc) ->
      raise (Fatal (Empty_TypeDestructorTriggerT (ALoc.to_loc_exn loc)))

    | _ -> super#on_t () t

  method! on_ReactElementConfigType () targ =
    match targ with
    | Ty.Fun _ -> raise (Fatal ReactElementConfigFunArg)
    | _ -> super#on_ReactElementConfigType () targ

  method! on_symbol env s =
    let { Ty.anonymous; def_loc; _ } = s in
    if anonymous then raise (Fatal (Anonymous (ALoc.to_loc_exn def_loc)));
    super#on_symbol env s
end

let validate_type = (new type_validator_visitor)#on_t ()

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
class mapper_type_printing_hardcoded_fixes = object(this)
  inherit [Loc.t] Flow_ast_mapper.mapper as super

  method private normalize_function ff =
    let open Flow_ast.Type.Function in
    let { params = (loc, { Params.params; rest }); _} = ff in
    let normalized_params_rev, _ = List.fold_left (fun (p, c) param ->
      match param with
      | loc, { Param.name = None; annot; optional } ->
        let normalized_param = loc,
          {Param.name = Some (Flow_ast_utils.ident_of_source (loc, Printf.sprintf "_%d" c));
           annot;
           optional }
        in
        (normalized_param :: p, c + 1)
      | _ -> (param :: p, c + 1)
    ) ([], 0) params in
    let normalized_params = List.rev normalized_params_rev in
    { ff with params = loc, { Params.params = normalized_params; rest } }

  method private type_generic_normalize (t: ('loc, 'loc) Flow_ast.Type.t) =
    super#type_ (match t with
    | (loc_f, Flow_ast.Type.Function ff) ->
      let nf = this#normalize_function ff in
      (loc_f, Flow_ast.Type.Function nf)
    | _ -> t)

  method! type_parameter_instantiation (pi: ('loc, 'loc) Flow_ast.Type.ParameterInstantiation.t) =
    let loc, targs = pi in
    let targs' = Core_list.map ~f:this#type_generic_normalize targs in
    if targs' == targs then pi
    else loc, targs'
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
class patch_up_react_mapper ?(imports_react=false) () = object (this)
  inherit [_] Ty.endo_ty as super
  method! on_t loc t =
    match t with

    (* If 'react' is not imported, then we treat the symbol as Remote, so that
     * it is imported with the same mechanism we import other Remote symbols.
     * Otherwise, we refer to these names as 'React.NAME'. *)
    | Ty.Generic ({
        Ty.name = (
          "AbstractComponent" | "ChildrenArray" | "ComponentType" | "Config" |
          "Context" | "Element" | "ElementConfig" | "ElementProps" | "ElementRef" |
          "ElementType" | "Key" | "Node" | "Portal" | "Ref" | "StatelessFunctionalComponent"
          ) as name;
        provenance = Ty_symbol.Library;
        def_loc;
        _} as symbol, kind, args_opt)
      when is_react_loc def_loc ->
      let args_opt = Flow_ast_mapper.map_opt (ListUtils.ident_map (this#on_t loc)) args_opt in
      let symbol = if imports_react
        then { symbol with Ty.name = "React." ^ name }
        else { symbol with Ty.provenance = Ty.Remote { Ty.imported_as = None } } in
      Ty.Generic (symbol, kind, args_opt)
    | _ -> super#on_t loc t

    method! on_prop loc prop =
      let prop = match prop with
        | Ty.NamedProp (name, named_prop)
          when Reason.is_internal_name name ->
          warn_shadow_prop name loc;
          (* Shadow props appear as regular props *)
          let name = String.sub name 1 (String.length name - 1) in
          Ty.NamedProp (name, named_prop)
        | prop -> prop
      in
      super#on_prop loc prop
end

(* Returns true if the location given a zero width location. *)
let is_point loc = Loc.(loc.start = loc._end)

let temporary_objectlit_symbol = {
  Ty.
  provenance = Ty.Builtin;
  def_loc = ALoc.none;
  name = "$TEMPORARY$object";
  anonymous = false;
}

let temporary_arraylit_symbol = {
  Ty.
  provenance = Ty.Builtin;
  def_loc = ALoc.none;
  name = "$TEMPORARY$array";
  anonymous = false;
}

module Builtins = struct
  let flowfixme =
    Ty.Generic (Ty_symbol.builtin_symbol "$FlowFixMe", Ty.TypeAliasKind, None)

  let flowfixme_empty =
    Ty.Generic (Ty_symbol.builtin_symbol "$FlowFixMeEmpty", Ty.TypeAliasKind, None)

  let empty =
    Ty.Bot Ty.EmptyType
end
