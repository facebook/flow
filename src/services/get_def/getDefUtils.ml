(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
module Result = Base.Result

let ( >>= ) = Result.( >>= )

let ( >>| ) = Result.( >>| )

let loc_of_aloc = Parsing_heaps.Reader.loc_of_aloc

module ObjectKeyAtLoc : sig
  (* Given a location, returns Some (enclosing_literal_loc, prop_loc, name) if the given location
   * points to an object literal key. The first location returned is the location for the entire
   * enclosing object literal. This is because later, we need to figure out which types are related
   * to this object literal which is easier to do when we have the location of the actual object
   * literal than if we only had the location of a single key. *)
  val get : (Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t -> (Loc.t * Loc.t * string) option
end = struct
  class object_key_finder target_loc =
    object (this)
      inherit [(Loc.t * Loc.t * string) option] Object_key_visitor.visitor ~init:None

      method! private visit_object_key
          (literal_loc : Loc.t) (key : (Loc.t, Loc.t) Flow_ast.Expression.Object.Property.key) =
        let open Flow_ast.Expression.Object in
        match key with
        | Property.Identifier (prop_loc, { Flow_ast.Identifier.name; comments = _ })
          when Loc.contains prop_loc target_loc ->
          this#set_acc (Some (literal_loc, prop_loc, name))
        | _ -> ()
    end

  let get ast target_loc =
    let finder = new object_key_finder target_loc in
    finder#eval finder#program ast
end

(* If the given type refers to an object literal, return the location of the object literal.
 * Otherwise return None *)
let get_object_literal_loc ~reader ty : Loc.t option =
  let open TypeUtil in
  let open Reason in
  let reason_desc = reason_of_t ty (* TODO look into unwrap *) |> desc_of_reason ~unwrap:false in
  match reason_desc with
  | RObjectLit -> Some (def_loc_of_t ty |> loc_of_aloc ~reader)
  | _ -> None

type def_kind =
  (* Use of a property, e.g. `foo.bar`. Includes type of receiver (`foo`) and name of the property
   * `bar` *)
  | Use of Type.t * string
  (* In a class, where a property/method is defined. Includes the type of the class and the name
     of the property. *)
  | Class_def of Type.t * string (* name *) * bool (* static *)
  (* In an object type. Includes the location of the property definition and its name. *)
  | Obj_def of Loc.t * string (* name *)
  (* List of types that the object literal flows into directly, as well as the name of the
   * property. *)
  | Use_in_literal of Type.t Nel.t * string

(* name *)

let set_def_loc_hook ~reader prop_access_info literal_key_info target_loc =
  let set_prop_access_info new_info =
    let set_ok info = prop_access_info := Ok (Some info) in
    let set_err err = prop_access_info := Error err in
    match !prop_access_info with
    | Error _ -> ()
    | Ok None -> prop_access_info := Ok (Some new_info)
    | Ok (Some info) ->
      begin
        match (info, new_info) with
        | (Use _, Use _)
        | (Class_def _, Class_def _)
        | (Obj_def _, Obj_def _) ->
          (* If we see hooks firing multiple times for the same
           * location, this is innocuous and we should take the last result.
           * Previously, this would occur due to generate-tests.
           *)
          set_ok new_info
        (* Literals can flow into multiple types. Include them all. *)
        | (Use_in_literal (types, name), Use_in_literal (new_types, new_name)) ->
          if name = new_name then
            set_ok (Use_in_literal (Nel.rev_append new_types types, name))
          else
            set_err "Names did not match"
        (* We should not see mismatches. *)
        | (Use _, _)
        | (Class_def _, _)
        | (Obj_def _, _)
        | (Use_in_literal _, _) ->
          set_err "Unexpected mismatch between definition kind"
      end
  in
  let use_hook ret _ctxt name loc ty =
    let loc = loc_of_aloc ~reader loc in
    if Loc.contains loc target_loc then set_prop_access_info (Use (ty, name));
    ret
  in
  let class_def_hook _ctxt ty static name loc =
    let loc = loc_of_aloc ~reader loc in
    if Loc.contains loc target_loc then set_prop_access_info (Class_def (ty, name, static))
  in
  let obj_def_hook _ctxt name loc =
    let loc = loc_of_aloc ~reader loc in
    if Loc.contains loc target_loc then set_prop_access_info (Obj_def (loc, name))
  in
  let export_named_hook name loc =
    let loc = loc_of_aloc ~reader loc in
    if Loc.contains loc target_loc then set_prop_access_info (Obj_def (loc, name))
  in
  let obj_to_obj_hook _ctxt obj1 obj2 =
    match (get_object_literal_loc ~reader obj1, literal_key_info) with
    | (Some loc, Some (target_loc, _, name)) when loc = target_loc ->
      Type.(
        begin
          match obj2 with
          | DefT (_, _, ObjT _) -> set_prop_access_info (Use_in_literal (Nel.one obj2, name))
          | _ -> ()
        end
      )
    | _ -> ()
  in
  Type_inference_hooks_js.set_member_hook (use_hook false);
  Type_inference_hooks_js.set_call_hook (use_hook ());
  Type_inference_hooks_js.set_class_member_decl_hook class_def_hook;
  Type_inference_hooks_js.set_obj_type_prop_decl_hook obj_def_hook;
  Type_inference_hooks_js.set_export_named_hook export_named_hook;
  Type_inference_hooks_js.set_obj_to_obj_hook obj_to_obj_hook

let unset_hooks () = Type_inference_hooks_js.reset_hooks ()

type single_def_info =
  | Class of Loc.t
  (* An object was found. *)
  | Object of Loc.t

(* If there are multiple relevant definition locations (e.g. the request was issued on an object
 * literal which is associated with multiple types) then there will be multiple locations in no
 * particular order. *)
type property_def_info = single_def_info Nel.t

type def_info =
  | Property of property_def_info * string (* name *)
  | CJSExport of Loc.t

let display_name_of_def_info = function
  | Property (_, name) -> name
  | CJSExport _ -> "module.exports"

let loc_of_single_def_info = function
  | Class loc -> loc
  | Object loc -> loc

let all_locs_of_property_def_info def_info = def_info |> Nel.map loc_of_single_def_info

let all_locs_of_def_info = function
  | Property (def_info, _) -> all_locs_of_property_def_info def_info
  | CJSExport loc -> Nel.one loc

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
  | Class loc -> spf "Class (%s)" (Loc.debug_to_string loc)
  | Object loc -> spf "Object (%s)" (Loc.debug_to_string loc)

let debug_string_of_property_def_info def_info =
  def_info
  |> Nel.map debug_string_of_single_def_info
  |> Nel.to_list
  |> String.concat ", "
  |> spf "[%s]"

let debug_string_of_def_info = function
  | Property (def_info, name) ->
    spf "Property (%s, %s)" (debug_string_of_property_def_info def_info) name
  | CJSExport loc -> spf "CJSExport (%s)" (Loc.debug_to_string loc)

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
    | ThisClassT (_, t, _)
    | DefT (_, _, PolyT { t_out = ThisClassT (_, t, _); _ }) ->
      Ok t
    | _ ->
      let type_string = string_of_ctor resolved in
      Error ("Expected a class type to extract an instance type from, got " ^ type_string)
  )

(* Must be called with the result from Members.extract_type *)
let get_def_loc_from_extracted_type cx extracted_type name =
  extracted_type |> Members.extract_members cx |> Members.to_command_result >>| fun map ->
  match SMap.find_opt name map with
  | None -> None
  (* Currently some types (e.g. spreads) do not contain locations for their properties. For now
   * we'll just treat them as if the properties do not exist, but once this is fixed this case
   * should be promoted to an error *)
  | Some (None, _) -> None
  | Some (Some loc, _) -> Some loc

let rec extract_def_loc ~reader cx ty name : (def_loc, string) result =
  let resolved = Members.resolve_type cx ty in
  extract_def_loc_resolved ~reader cx resolved name

(* The same as get_def_loc_from_extracted_type except it recursively checks for overridden
 * definitions of the member in superclasses and returns those as well *)
and extract_def_loc_from_instancet ~reader cx extracted_type super name : (def_loc, string) result =
  let current_class_def_loc = get_def_loc_from_extracted_type cx extracted_type name in
  current_class_def_loc >>= function
  | None -> Ok NoDefFound
  | Some loc ->
    let loc = loc_of_aloc ~reader loc in
    extract_def_loc ~reader cx super name >>= ( function
    | FoundClass lst ->
      (* Avoid duplicate entries. This can happen if a class does not override a method,
       * so the definition points to the method definition in the parent class. Then we
       * look at the parent class and find the same definition. *)
      let lst =
        if Nel.hd lst = loc then
          lst
        else
          Nel.cons loc lst
      in
      Ok (FoundClass lst)
    | FoundObject _ -> Error "A superclass should be a class, not an object"
    | FoundUnion _ -> Error "A superclass should be a class, not a union"
    (* If the superclass does not have a definition for this method, or it is for some reason
     * not a class type, or we don't know its type, just return the location we already know
     * about. *)
    | NoDefFound
    | UnsupportedType
    | AnyType ->
      Ok (FoundClass (Nel.one loc)) )

and extract_def_loc_resolved ~reader cx ty name : (def_loc, string) result =
  Members.(
    Type.(
      match extract_type cx ty with
      | Success (DefT (_, _, InstanceT (_, super, _, _))) as extracted_type ->
        extract_def_loc_from_instancet ~reader cx extracted_type super name
      | (Success (DefT (_, _, ObjT _)) | SuccessModule _) as extracted_type ->
        get_def_loc_from_extracted_type cx extracted_type name >>| ( function
        | None -> NoDefFound
        | Some loc -> FoundObject (loc_of_aloc ~reader loc) )
      | Success (UnionT (_, rep)) ->
        let union_members =
          UnionRep.members rep
          |> Base.List.map ~f:(fun member -> extract_def_loc ~reader cx member name)
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

(* Takes the file key where the module reference appeared, as well as the module reference, and
 * returns the file name for the module that the module reference refers to. *)
let file_key_of_module_ref ~reader file_key module_ref =
  let resolved =
    Module_js.find_resolved_module
      ~reader:(Abstract_state_reader.State_reader reader)
      ~audit:Expensive.warn
      file_key
      module_ref
  in
  Module_heaps.Reader.get_file ~reader ~audit:Expensive.warn resolved

let def_info_of_typecheck_results ~reader cx props_access_info =
  let def_info_of_class_member_locs locs =
    (* We want to include the immediate implementation as well as all superclass implementations.
     * If we wanted a mode where superclass implementations were not included, for example, we
     * could choose to take only the first extracted location. *)
    Nel.map (fun loc -> Class loc) locs
  in
  let def_info_of_type name ty =
    let rec def_info_of_def_loc = function
      | FoundClass locs -> Some (def_info_of_class_member_locs locs)
      | FoundObject loc -> Some (Nel.one (Object loc))
      | FoundUnion def_locs ->
        def_locs |> Nel.map def_info_of_def_loc |> Nel.cat_maybes |> Base.Option.map ~f:Nel.concat
      | NoDefFound
      | UnsupportedType
      | AnyType ->
        None
    in
    extract_def_loc ~reader cx ty name >>| def_info_of_def_loc
  in
  match props_access_info with
  | None -> Ok None
  | Some (Obj_def (loc, name)) -> Ok (Some (Nel.one (Object loc), name))
  | Some (Class_def (ty, name, static)) ->
    if static then
      (* Here, `ty` ends up resolving to `ObjT` so we lose the knowledge that this is a static
       * property. This means that we don't get the fancy look-up-the-inheritance-chain behavior
       * that we get with class instances. That would be nice to add at some point. *)
      def_info_of_type name ty >>| Base.Option.map ~f:(fun def_info -> (def_info, name))
    else
      (* We get the type of the class back here, so we need to extract the type of an instance *)
      extract_instancet cx ty >>= fun ty ->
      extract_def_loc_resolved ~reader cx ty name >>= ( function
      | FoundClass locs -> Ok (Some (def_info_of_class_member_locs locs, name))
      | FoundUnion _
      | FoundObject _ ->
        Error "Expected to extract class def info from a class"
      | _ -> Error "Unexpectedly failed to extract definition from known type" )
  | Some (Use (ty, name)) ->
    def_info_of_type name ty >>| Base.Option.map ~f:(fun def_info -> (def_info, name))
  | Some (Use_in_literal (types, name)) ->
    let def_infos_result = Nel.map (def_info_of_type name) types |> Nel.result_all in
    def_infos_result >>| fun def_infos ->
    Nel.cat_maybes def_infos
    |> Base.Option.map ~f:Nel.concat
    |> Base.Option.map ~f:(fun def_info -> (def_info, name))

let add_literal_properties literal_key_info def_info =
  (* If we happen to be on an object property, include the location of that
   * property as a def loc. We don't want to do that above because:
   * (a) We could also encounter a `Use_in_literal` if this object literal flows
   * into another object type. This would force us to make props_access_info a
   * list and add additional complexity just for the sake of this one case.
   * (b) We would have to add a type inference hook, which we are trying to
   * avoid. *)
  let def_info =
    match (def_info, literal_key_info) with
    | (None, None) -> Ok None
    | (Some _, None) -> Ok def_info
    | (None, Some (_, loc, name)) -> Ok (Some (Nel.one (Object loc), name))
    | (Some (defs, name1), Some (_, loc, name2)) ->
      if name1 <> name2 then
        Error "Unexpected name mismatch"
      else
        Ok (Some (Nel.cons (Object loc) defs, name1))
  in
  Result.map
    def_info
    ~f:(Base.Option.map ~f:(fun (prop_def_info, name) -> Property (prop_def_info, name)))

let get_def_info ~reader ~options env profiling file_key ast_info loc :
    (def_info option, string) result Lwt.t =
  let props_access_info = ref (Ok None) in
  let (ast, file_sig, info) = ast_info in
  (* Check if it's an exported symbol *)
  let loc = Base.Option.value (ImportExportSymbols.find_related_symbol file_sig loc) ~default:loc in
  let info = Docblock.set_flow_mode_for_ide_command info in
  let literal_key_info : (Loc.t * Loc.t * string) option = ObjectKeyAtLoc.get ast loc in
  let%lwt cx =
    set_def_loc_hook ~reader props_access_info literal_key_info loc;
    Profiling_js.with_timer_lwt profiling ~timer:"MergeContents" ~f:(fun () ->
        let%lwt () =
          Type_contents.ensure_checked_dependencies ~options ~reader ~env file_key file_sig
        in
        let (cx, _) =
          Merge_service.check_contents_context ~reader options file_key ast info file_sig
        in
        Lwt.return cx
    )
  in
  unset_hooks ();
  !props_access_info %>>= fun props_access_info ->
  let def_info = def_info_of_typecheck_results ~reader cx props_access_info in
  let def_info = def_info >>= add_literal_properties literal_key_info in
  let def_info =
    def_info >>= function
    | Some _ as def_info -> Ok def_info
    | None ->
      (* Check if we are on a CJS import/export. These cases are not covered above since the type
       * system hooks don't quite get us what we want. *)
      let export_loc =
        File_sig.With_Loc.(
          List.fold_left
            begin
              fun acc -> function
              | Require { source = (_, module_ref); require_loc; _ } ->
                if Loc.contains require_loc loc then
                  match acc with
                  | Error _ -> acc
                  | Ok (Some _) -> Error "Did not expect multiple requires to match one location"
                  | Ok None ->
                    let external_file_sig =
                      let filename = file_key_of_module_ref ~reader file_key module_ref in
                      Base.Option.bind filename (Parsing_heaps.Reader.get_file_sig ~reader)
                    in
                    Result.return
                    @@ Base.Option.bind external_file_sig (fun external_file_sig ->
                           match external_file_sig.module_sig.module_kind with
                           | CommonJS { mod_exp_loc = Some loc; _ } -> Some loc
                           | _ -> None
                       )
                else
                  acc
              | _ -> acc
            end
            (Ok None)
            file_sig.module_sig.requires
        )
      in
      let export_loc =
        export_loc >>| function
        | Some _ as x -> x
        | None ->
          File_sig.With_Loc.(
            (match file_sig.module_sig.module_kind with
            | CommonJS { mod_exp_loc = Some mod_exp_loc; _ } ->
              if Loc.contains mod_exp_loc loc then
                Some mod_exp_loc
              else
                None
            | _ -> None)
          )
      in
      Result.map export_loc ~f:(Base.Option.map ~f:(fun x -> CJSExport x))
  in
  Lwt.return @@ def_info
