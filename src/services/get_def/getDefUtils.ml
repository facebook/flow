(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Get_def_types
module Result = Base.Result

let ( >>= ) = Result.( >>= )

let ( >>| ) = Result.( >>| )

(* If the given type refers to an object literal, return the location of the object literal.
 * Otherwise return None *)
let get_object_literal_loc ty : ALoc.t option =
  let open TypeUtil in
  let open Reason in
  let reason_desc = reason_of_t ty (* TODO look into unwrap *) |> desc_of_reason ~unwrap:false in
  match reason_desc with
  | RObjectLit -> Some (def_loc_of_t ty)
  | _ -> None

type 'loc def_kind =
  | Use of Type.t * (* name *) string
      (** Use of a property, e.g. `foo.bar`. Includes type of receiver (`foo`) and name of the
          property `bar` *)
  | Class_def of Type.t * (* name *) string * (* static *) bool
      (** In a class, where a property/method is defined. Includes the type of the class and the name
          of the property. *)
  | Obj_def of 'loc * (* name *) string
      (** In an object type. Includes the location of the property definition and its name. *)
  | Obj_literal of 'loc * (* type of the object *) Type.t * (* name *) string
      (** In an object literal. Includes the location of the property definition, the object type,
          and the prop name. This object does not necessarily contain the desired property/method *)
  | PrivateName of {
      def_loc: 'loc;
      references: 'loc list;
      name: string;
    }

let map_def_kind_loc ~f = function
  | Use (t, name) -> Use (t, name)
  | Class_def (t, name, static) -> Class_def (t, name, static)
  | Obj_def (loc, name) -> Obj_def (f loc, name)
  | Obj_literal (loc, t, name) -> Obj_literal (f loc, t, name)
  | PrivateName { def_loc; references; name } ->
    PrivateName { def_loc = f def_loc; references = List.map f references; name }

module Def_kind_search = struct
  exception
    Found_class_def of {
      name: string;
      static: bool;
    }

  exception
    Found_obj_prop of {
      loc: ALoc.t;
      name: string;
    }

  exception Found_import of { name: string }

  exception Found of ALoc.t def_kind

  type available_private_name = {
    def_loc: ALoc.t;
    mutable references: ALoc.t list;
    mutable has_covered_target_reference: bool;
  }

  class searcher ~(covers_target : ALoc.t -> bool) =
    object (this)
      inherit
        [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

      val mutable available_private_names : available_private_name SMap.t = SMap.empty

      method on_loc_annot x = x

      method on_type_annot x = x

      method! expression (((l, t), expr) as x) =
        let open Flow_ast.Expression in
        match expr with
        | Class _ ->
          (try super#expression x with
          | Found_class_def { name; static } -> raise (Found (Class_def (t, name, static))))
        | Object _ ->
          (try super#expression x with
          | Found_obj_prop { name; loc = _ } -> raise (Found (Obj_literal (l, t, name))))
        | _ -> super#expression x

      method! class_declaration cls =
        let open Flow_ast.Class in
        let { id; _ } = cls in
        let class_type =
          match id with
          | Some ((_, annot), _) -> annot
          | None -> failwith "Class declaration must have an id"
        in
        try super#class_declaration cls with
        | Found_class_def { name; static } -> raise (Found (Class_def (class_type, name, static)))

      method! class_body cls_body =
        let open Flow_ast.Class in
        let (_, { Body.body; comments = _ }) = cls_body in
        let (all_available_private_names, new_available_private_names) =
          let add_private_name (all, new_) (def_loc, { Flow_ast.PrivateName.name; _ }) =
            let entry = { def_loc; references = []; has_covered_target_reference = false } in
            (SMap.add name entry all, SMap.add name entry new_)
          in
          Base.List.fold body ~init:(available_private_names, SMap.empty) ~f:(fun acc -> function
            | Body.Method
                (_, { Method.key = Flow_ast.Expression.Object.Property.PrivateName name; _ })
            | Body.PrivateField (_, { PrivateField.key = name; _ }) ->
              add_private_name acc name
            | Body.Method _ -> acc
            | Body.Property _ -> acc
          )
        in
        let saved_available_private_names = available_private_names in
        available_private_names <- all_available_private_names;
        let result =
          Base.Result.try_with (fun () ->
              let r = super#class_body cls_body in
              SMap.iter
                (fun name { def_loc; references; has_covered_target_reference } ->
                  if has_covered_target_reference then
                    raise (Found (PrivateName { def_loc; references; name })))
                new_available_private_names;
              r
          )
        in
        available_private_names <- saved_available_private_names;
        Base.Result.ok_exn result

      method! private_name ((loc, { Flow_ast.PrivateName.name; comments = _ }) as pn) =
        match SMap.find_opt name available_private_names with
        | Some entry ->
          entry.has_covered_target_reference <-
            entry.has_covered_target_reference || covers_target loc;
          if loc <> entry.def_loc then entry.references <- loc :: entry.references;
          pn
        | None -> pn

      method private visit_class_key ~static key =
        let open Flow_ast.Expression.Object.Property in
        match key with
        | Identifier ((loc, _), { Flow_ast.Identifier.name; _ }) ->
          if covers_target loc then raise (Found_class_def { name; static })
        | _ -> ()

      method! class_method prop =
        let open Flow_ast.Class.Method in
        let { key; static; _ } = prop in
        this#visit_class_key ~static key;
        super#class_method prop

      method! class_property prop =
        let open Flow_ast.Class.Property in
        let { key; static; _ } = prop in
        this#visit_class_key ~static key;
        super#class_property prop

      method! import_declaration loc decl =
        let open Flow_ast.Statement.ImportDeclaration in
        try super#import_declaration loc decl with
        | Found_import { name } ->
          let { source = ((_, module_t), _); _ } = decl in
          raise (Found (Use (module_t, name)))

      method! import_default_specifier ~import_kind:_ id =
        let ((loc, _), _) = id in
        if covers_target loc then raise (Found_import { name = "default" });
        id

      method! import_named_specifier ~import_kind:_ specifier =
        let open Flow_ast.Statement.ImportDeclaration in
        let { kind = _; local; remote; remote_name_def_loc = _ } = specifier in
        let ((loc, _), { Flow_ast.Identifier.name; _ }) = Base.Option.value ~default:remote local in
        if covers_target loc then raise (Found_import { name });
        specifier

      method! export_default_declaration loc decl =
        let open Flow_ast.Statement.ExportDefaultDeclaration in
        let { default = (default_loc, _); _ } = decl in
        if covers_target default_loc then raise (Found (Obj_def (default_loc, "default")));
        super#export_default_declaration loc decl

      method! export_named_declaration loc decl =
        let { Flow_ast.Statement.ExportNamedDeclaration.declaration; _ } = decl in
        (match declaration with
        | Some (_, stmt) ->
          let open Flow_ast.Statement in
          (match stmt with
          | FunctionDeclaration { Flow_ast.Function.id = Some id; _ }
          | ClassDeclaration { Flow_ast.Class.id = Some id; _ }
          | TypeAlias { TypeAlias.id; _ }
          | OpaqueType { OpaqueType.id; _ }
          | InterfaceDeclaration { Interface.id; _ }
          | EnumDeclaration { EnumDeclaration.id; _ } ->
            let ((id_loc, _), { Flow_ast.Identifier.name; comments = _ }) = id in
            if covers_target id_loc then raise (Found (Obj_def (id_loc, name)))
          | VariableDeclaration { VariableDeclaration.declarations; _ } ->
            Flow_ast_utils.fold_bindings_of_variable_declarations
              (fun _has_annot () id ->
                let ((id_loc, _), { Flow_ast.Identifier.name; comments = _ }) = id in
                if covers_target id_loc then raise (Found (Obj_def (id_loc, name))))
              ()
              declarations
          | _ -> ())
        | None -> ());
        super#export_named_declaration loc decl

      method! member expr =
        let open Flow_ast.Expression.Member in
        let { _object; property; comments = _ } = expr in
        (match property with
        | PropertyIdentifier ((id_loc, _), { Flow_ast.Identifier.name; _ }) ->
          if covers_target id_loc then
            let ((_, obj_t), _) = _object in
            raise (Found (Use (obj_t, name)))
        | PropertyPrivateName _
        | PropertyExpression _ ->
          ());
        super#member expr

      method! object_property prop =
        let open Flow_ast.Expression.Object.Property in
        let (_prop_loc, prop') = prop in
        let key =
          match prop' with
          | Init { key; _ }
          | Method { key; _ }
          | Get { key; _ }
          | Set { key; _ } ->
            key
        in
        match key with
        | Identifier ((id_loc, _), { Flow_ast.Identifier.name; comments = _ })
          when covers_target id_loc ->
          raise (Found_obj_prop { loc = id_loc; name })
        | _ -> super#object_property prop

      method! object_property_type prop =
        let open Flow_ast.Expression.Object.Property in
        let (_, { Flow_ast.Type.Object.Property.key; _ }) = prop in
        (match key with
        | Identifier ((loc, _), { Flow_ast.Identifier.name; _ }) ->
          if covers_target loc then raise (Found (Obj_def (loc, name)))
        | _ -> ());
        super#object_property_type prop

      method! pattern ?kind expr =
        let open Flow_ast.Pattern in
        let ((_, ty), patt) = expr in
        (match patt with
        | Object { Object.properties; _ } ->
          List.iter
            (function
              | Object.Property
                  ( _,
                    {
                      Object.Property.key =
                        Object.Property.Identifier ((loc, _), { Flow_ast.Identifier.name; _ });
                      _;
                    }
                  )
                when covers_target loc ->
                raise (Found (Use (ty, name)))
              | _ -> ())
            properties
        | _ -> ());
        super#pattern ?kind expr
    end

  let search ~f ast =
    let s = new searcher ~covers_target:f in
    try
      let _ = s#program ast in
      None
    with
    | Found def_kind -> Some def_kind
end

let loc_of_single_def_info = function
  | ClassProperty loc -> loc
  | ObjectProperty loc -> loc

let all_locs_of_ordinary_property_def_info props_info = props_info |> Nel.map loc_of_single_def_info

let all_locs_of_def_info = function
  | VariableDefinition (locs, _) -> locs
  | PropertyDefinition (OrdinaryProperty { props_info; name = _ }) ->
    props_info |> all_locs_of_ordinary_property_def_info |> Nel.to_list
  | PropertyDefinition (PrivateNameProperty { def_loc; _ }) -> [def_loc]
  | NoDefinition _ -> []

type def_loc =
  (* We found a class property. Include all overridden implementations. Superclass implementations
   * are listed last. *)
  | FoundClass of Loc.t Nel.t
  (* We found an object property. *)
  | FoundObject of Loc.t
  | FoundUnion of def_loc Nel.t
  (* This means we resolved the receiver type but did not find the definition. If this happens
   * there must be a type error (which may be suppresssed) *)
  | NoDefFound
  (* This means it's a known type that we deliberately do not currently support. *)
  | UnsupportedType
  (* This means it's not well-typed, and could be anything *)
  | AnyType

let debug_string_of_locs locs =
  locs |> Nel.to_list |> Base.List.map ~f:Loc.debug_to_string |> String.concat ", "

(* Disable the unused value warning -- we want to keep this around for debugging *)
[@@@warning "-32"]

let debug_string_of_single_def_info = function
  | ClassProperty loc -> spf "ClassProperty (%s)" (Loc.debug_to_string loc)
  | ObjectProperty loc -> spf "ObjectProperty (%s)" (Loc.debug_to_string loc)

let debug_string_of_property_def_info def_info =
  def_info
  |> Nel.map debug_string_of_single_def_info
  |> Nel.to_list
  |> String.concat ", "
  |> spf "[%s]"

let debug_string_of_def_info (def_info, name) =
  spf "(%s, %s)" (debug_string_of_property_def_info def_info) name

let rec debug_string_of_def_loc = function
  | FoundClass locs -> spf "FoundClass (%s)" (debug_string_of_locs locs)
  | FoundObject loc -> spf "FoundObject (%s)" (Loc.debug_to_string loc)
  | FoundUnion def_locs ->
    Nel.to_list def_locs
    |> Base.List.map ~f:debug_string_of_def_loc
    |> String.concat ", "
    |> spf "FoundUnion (%s)"
  | NoDefFound -> "NoDefFound"
  | UnsupportedType -> "UnsupportedType"
  | AnyType -> "AnyType"

(* Re-enable the unused value warning *)
[@@@warning "+32"]

let extract_instancet cx ty : (Type.t, string) result =
  Type.(
    let resolved = Members.resolve_type cx ty in
    match resolved with
    | DefT (_, ClassT (ThisInstanceT (r, t, _, _)))
    | DefT (_, PolyT { t_out = DefT (_, ClassT (ThisInstanceT (r, t, _, _))); _ }) ->
      Ok (DefT (r, InstanceT t))
    | _ ->
      let type_string = string_of_ctor resolved in
      Error ("Expected a class type to extract an instance type from, got " ^ type_string)
  )

(* Must be called with the result from Members.extract_type *)
let get_def_locs_from_extracted_type cx extracted_type name =
  extracted_type |> Members.extract_members cx |> Members.to_command_result >>| fun map ->
  match SMap.find_opt name map with
  | None -> None
  (* Currently some types (e.g. spreads) do not contain locations for their properties. For now
   * we'll just treat them as if the properties do not exist, but once this is fixed this case
   * should be promoted to an error *)
  | Some (None, _) -> None
  | Some (Some loc, _) -> Some loc

let rec extract_def_loc ~loc_of_aloc cx ty name : (def_loc, string) result =
  let resolved = Members.resolve_type cx ty in
  extract_def_loc_resolved ~loc_of_aloc cx resolved name

(* The same as get_def_loc_from_extracted_type except it recursively checks for overridden
 * definitions of the member in superclasses and returns those as well *)
and extract_def_locs_from_instancet ~loc_of_aloc cx extracted_type super name :
    (def_loc, string) result =
  let current_class_def_locs = get_def_locs_from_extracted_type cx extracted_type name in
  current_class_def_locs >>= function
  | None -> Ok NoDefFound
  | Some locs ->
    let locs = Nel.map loc_of_aloc locs in
    let map_found_class ~f locs =
      match locs with
      | (loc, []) -> Ok (FoundClass (f loc))
      | locs -> Ok (FoundUnion (Nel.map (fun loc -> FoundClass (f loc)) locs))
    in
    extract_def_loc ~loc_of_aloc cx super name >>= ( function
    | FoundClass lst ->
      (* Avoid duplicate entries. This can happen if a class does not override a method,
       * so the definition points to the method definition in the parent class. Then we
       * look at the parent class and find the same definition. *)
      let add loc =
        if Nel.hd lst = loc then
          lst
        else
          Nel.cons loc lst
      in
      map_found_class ~f:add locs
    | FoundObject _ -> Error "A superclass should be a class, not an object"
    | FoundUnion _ -> Error "A superclass should be a class, not a union"
    (* If the superclass does not have a definition for this method, or it is for some reason
     * not a class type, or we don't know its type, just return the location we already know
     * about. *)
    | NoDefFound
    | UnsupportedType
    | AnyType ->
      map_found_class ~f:Nel.one locs )

and extract_def_loc_resolved ~loc_of_aloc cx ty name : (def_loc, string) result =
  Members.(
    Type.(
      match extract_type cx ty with
      | Success (DefT (_, InstanceT { super; _ }) | ThisInstanceT (_, { super; _ }, _, _)) as
        extracted_type ->
        extract_def_locs_from_instancet ~loc_of_aloc cx extracted_type super name
      | (Success (DefT (_, ObjT _)) | SuccessModule _ | SuccessNamespace _) as extracted_type ->
        get_def_locs_from_extracted_type cx extracted_type name >>| ( function
        | None -> NoDefFound
        | Some (loc, []) -> FoundObject (loc_of_aloc loc)
        | Some locs -> FoundUnion (Nel.map (fun loc -> FoundObject (loc_of_aloc loc)) locs) )
      | Success (UnionT (_, rep)) ->
        let union_members =
          UnionRep.members rep
          |> Base.List.map ~f:(fun member -> extract_def_loc ~loc_of_aloc cx member name)
          |> Result.all
        in
        ( union_members >>= fun members ->
          Nel.of_list members |> Result.of_option ~error:"Union should have at least one member"
        )
        >>| fun members_nel -> FoundUnion members_nel
      | Success _
      | FailureNullishType
      | FailureUnhandledType _
      | FailureUnhandledMembers _ ->
        Ok UnsupportedType
      | FailureAnyType -> Ok AnyType
    )
  )

let get_loc_of_prop ~loc_of_aloc props name =
  match NameUtils.Map.find_opt (Reason.OrdinaryName name) props with
  | Some prop ->
    (match Type.Property.read_loc prop with
    | Some aloc -> Some (loc_of_aloc aloc)
    | None -> None)
  | None -> None

let def_info_of_typecheck_results ~loc_of_aloc cx obj_to_obj_map props_access_info =
  let def_info_of_class_member_locs locs =
    (* We want to include the immediate implementation as well as all superclass implementations.
     * If we wanted a mode where superclass implementations were not included, for example, we
     * could choose to take only the first extracted location. *)
    Nel.map (fun loc -> ClassProperty loc) locs
  in
  let def_info_of_type name ty =
    let rec def_info_of_def_loc = function
      | FoundClass locs -> Some (def_info_of_class_member_locs locs)
      | FoundObject loc -> Some (Nel.one (ObjectProperty loc))
      | FoundUnion def_locs ->
        def_locs |> Nel.map def_info_of_def_loc |> Nel.cat_maybes |> Base.Option.map ~f:Nel.concat
      | NoDefFound
      | UnsupportedType
      | AnyType ->
        None
    in
    extract_def_loc ~loc_of_aloc cx ty name >>| def_info_of_def_loc
  in
  match props_access_info with
  | Obj_def (loc, name) ->
    Ok (Some (OrdinaryProperty { props_info = Nel.one (ObjectProperty loc); name }))
  | Class_def (ty, name, static) ->
    if static then
      (* Here, `ty` ends up resolving to `ObjT` so we lose the knowledge that this is a static
       * property. This means that we don't get the fancy look-up-the-inheritance-chain behavior
       * that we get with class instances. That would be nice to add at some point. *)
      def_info_of_type name ty
      >>| Base.Option.map ~f:(fun def_info -> OrdinaryProperty { props_info = def_info; name })
    else
      (* We get the type of the class back here, so we need to extract the type of an instance *)
      extract_instancet cx ty >>= fun ty ->
      extract_def_loc_resolved ~loc_of_aloc cx ty name >>= ( function
      | FoundClass locs ->
        Ok (Some (OrdinaryProperty { props_info = def_info_of_class_member_locs locs; name }))
      | FoundUnion _
      | FoundObject _ ->
        Error "Expected to extract class def info from a class"
      | _ -> Error "Unexpectedly failed to extract definition from known type" )
  | Use (ty, name) ->
    def_info_of_type name ty
    >>| Base.Option.map ~f:(fun def_info -> OrdinaryProperty { props_info = def_info; name })
  | Obj_literal (loc, ty, name) ->
    (match ty with
    | Type.(DefT (_, ObjT { props_tmap = literal_obj_props_tmap_id; _ })) ->
      let literal_props = Context.find_props cx literal_obj_props_tmap_id in
      let literal_result =
        match get_loc_of_prop ~loc_of_aloc literal_props name with
        | Some loc -> Ok (Nel.one (ObjectProperty loc))
        | None -> Error "Expected to find property on object definition"
      in
      let result =
        literal_result >>= fun literal_result ->
        (* Look up the objects that this object maps to *)
        match Loc_collections.LocMap.find_opt loc obj_to_obj_map with
        | Some obj_prop_tmap_ids ->
          Type.Properties.Set.fold
            (fun props_tmap_set acc ->
              let props = Context.find_props cx props_tmap_set in
              (* Get the loc of the specific prop def *)
              match get_loc_of_prop ~loc_of_aloc props name with
              | Some loc -> Base.Result.map ~f:(fun acc' -> Nel.cons (ObjectProperty loc) acc') acc
              | None -> Error "Expected to find property on object definition")
            obj_prop_tmap_ids
            (Ok literal_result)
        | None ->
          (* object literal has no upper bound objects *)
          Ok literal_result
      in
      Base.Result.map ~f:(fun res -> Some (OrdinaryProperty { props_info = res; name })) result
    | _ -> Error "Expected to find an object")
  | PrivateName { def_loc; references; name } ->
    Ok (Some (PrivateNameProperty { def_loc; references; name }))

let get_property_def_info ~loc_of_aloc type_info loc : (property_def_info option, string) result =
  let (Types_js_types.Typecheck_artifacts { cx; typed_ast; obj_to_obj_map }) = type_info in
  let def_kind =
    Def_kind_search.search
      ~f:(fun aloc ->
        let l = loc_of_aloc aloc in
        Loc.contains l loc)
      typed_ast
    |> Base.Option.map ~f:(map_def_kind_loc ~f:loc_of_aloc)
  in

  Base.Option.value_map
    ~f:(def_info_of_typecheck_results ~loc_of_aloc cx obj_to_obj_map)
    ~default:(Ok None)
    def_kind

let get_def_info ~options ~reader ~purpose (ast, file_sig, _) type_info loc :
    (def_info, string) result =
  let (Types_js_types.Typecheck_artifacts { cx; typed_ast; obj_to_obj_map = _ }) = type_info in
  let loc_of_aloc = Parsing_heaps.Reader.loc_of_aloc ~reader in
  match get_property_def_info ~loc_of_aloc type_info loc with
  | Error error -> Error error
  | Ok (Some props_info) -> Ok (PropertyDefinition props_info)
  | Ok None ->
    (match GetDef_js.get_def ~options ~loc_of_aloc ~cx ~file_sig ~ast ~typed_ast ~purpose loc with
    | GetDef_js.Get_def_result.Def (locs, name)
    | GetDef_js.Get_def_result.Partial (locs, name, _) ->
      Ok (VariableDefinition (Loc_collections.LocSet.elements locs, name))
    | GetDef_js.Get_def_result.Bad_loc error -> Ok (NoDefinition (Some error))
    | GetDef_js.Get_def_result.Def_error error -> Error error)
