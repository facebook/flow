(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open ServerEnv

module Result = Core_result
let (>>=) = Result.(>>=)
let (>>|) = Result.(>>|)

open FindRefsUtils

let add_ref_kind kind = List.map (fun loc -> (kind, loc))

(* The default visitor does not provide all of the context we need when visiting an object key. In
 * particular, we need the location of the enclosing object literal. *)
class ['acc] object_key_visitor ~init = object(this)
  inherit ['acc] Flow_ast_visitor.visitor ~init as super

  method! expression (exp: (Loc.t, Loc.t) Ast.Expression.t) =
    let open Ast.Expression in
    begin match exp with
    | loc, Object x ->
      this#visit_object_literal loc x
    | _ -> ()
    end;
    super#expression exp

  method private visit_object_literal (loc: Loc.t) (obj: (Loc.t, Loc.t) Ast.Expression.Object.t) =
    let open Ast.Expression.Object in
    let get_prop_key =
      let open Property in
      function Init { key; _ } | Method { key; _ } | Get { key; _ } | Set { key; _ } -> key
    in
    let { properties } = obj in
    properties
    |> List.iter begin function
      | SpreadProperty _ -> ()
      | Property (_, prop) -> prop |> get_prop_key |> this#visit_object_key loc
    end

  method private visit_object_key
      (_literal_loc: Loc.t)
      (_key: (Loc.t, Loc.t) Ast.Expression.Object.Property.key) =
    ()
end

module ObjectKeyAtLoc : sig
  (* Given a location, returns Some (enclosing_literal_loc, prop_loc, name) if the given location
   * points to an object literal key. The first location returned is the location for the entire
   * enclosing object literal. This is because later, we need to figure out which types are related
   * to this object literal which is easier to do when we have the location of the actual object
   * literal than if we only had the location of a single key. *)
  val get: (Loc.t, Loc.t) Ast.program -> Loc.t -> (Loc.t * Loc.t * string) option
end = struct
  class object_key_finder target_loc = object(this)
    inherit [(Loc.t * Loc.t * string) option] object_key_visitor ~init:None
    method! private visit_object_key
        (literal_loc: Loc.t)
        (key: (Loc.t, Loc.t) Ast.Expression.Object.Property.key) =
      let open Ast.Expression.Object in
      match key with
      | Property.Identifier (prop_loc, name) when Loc.contains prop_loc target_loc ->
        this#set_acc (Some (literal_loc, prop_loc, name))
      | _ -> ()
  end

  let get ast target_loc =
    let finder = new object_key_finder target_loc in
    finder#eval finder#program ast
end

module LiteralToPropLoc : sig
  (* Returns a map from object_literal_loc to prop_loc, for all object literals which contain the
   * given property name. *)
  val make: (Loc.t, Loc.t) Ast.program -> prop_name: string -> Loc.t LocMap.t
end = struct
  class locmap_builder prop_name = object(this)
    inherit [Loc.t LocMap.t] object_key_visitor ~init:LocMap.empty
    method! private visit_object_key
        (literal_loc: Loc.t)
        (key: (Loc.t, Loc.t) Ast.Expression.Object.Property.key) =
      let open Ast.Expression.Object in
      match key with
      | Property.Identifier (prop_loc, name) when name = prop_name ->
          this#update_acc (fun map -> LocMap.add literal_loc prop_loc map)
        (* TODO consider supporting other property keys (e.g. literals). Also update the
         * optimization in property_access_searcher below when this happens. *)
      | _ -> ()
  end

  let make ast ~prop_name =
    let builder = new locmap_builder prop_name in
    builder#eval builder#program ast
end

(* If the given type refers to an object literal, return the location of the object literal.
 * Otherwise return None *)
let get_object_literal_loc ty : Loc.t option =
  let open Type in
  let open Reason in
  let reason_desc =
    reason_of_t ty
    (* TODO look into unwrap *)
    |> desc_of_reason ~unwrap:false
  in
  match reason_desc with
  | RObjectLit -> Some (Type.def_loc_of_t ty)
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
  | Use_in_literal of Type.t Nel.t * string (* name *)

let set_def_loc_hook prop_access_info literal_key_info target_loc =
  let set_prop_access_info new_info =
    let set_ok info = prop_access_info := Ok (Some info) in
    let set_err err = prop_access_info := Error err in
    match !prop_access_info with
      | Error _ -> ()
      | Ok None -> prop_access_info := Ok (Some new_info)
      | Ok (Some info) -> begin match info, new_info with
        | Use _, Use _
        | Class_def _, Class_def _
        | Obj_def _, Obj_def _ ->
          (* Due to generate_tests, we sometimes see hooks firing multiple times for the same
           * location. This is innocuous and we should take the last result. *)
          set_ok new_info
        (* Literals can flow into multiple types. Include them all. *)
        | Use_in_literal (types, name), Use_in_literal (new_types, new_name) ->
          if name = new_name then
            set_ok (Use_in_literal (Nel.rev_append new_types types, name))
          else
            set_err "Names did not match"
        (* We should not see mismatches. *)
        |  Use _, _ | Class_def _, _ | Obj_def _, _ | Use_in_literal _, _ ->
          set_err "Unexpected mismatch between definition kind"
      end
  in
  let use_hook ret _ctxt name loc ty =
    begin if Loc.contains loc target_loc then
      set_prop_access_info (Use (ty, name))
    end;
    ret
  in
  let class_def_hook _ctxt ty static name loc =
    if Loc.contains loc target_loc then
      set_prop_access_info (Class_def (ty, name, static))
  in
  let obj_def_hook _ctxt name loc =
    if Loc.contains loc target_loc then
      set_prop_access_info (Obj_def (loc, name))
  in
  let export_named_hook name loc =
    if Loc.contains loc target_loc then
      set_prop_access_info (Obj_def (loc, name))
  in
  let obj_to_obj_hook _ctxt obj1 obj2 =
    match get_object_literal_loc obj1, literal_key_info with
    | Some loc, Some (target_loc, _, name) when loc = target_loc ->
      let open Type in
      begin match obj2 with
      | DefT (_, ObjT _) ->
        set_prop_access_info (Use_in_literal (Nel.one obj2, name))
      | _ -> ()
      end
    | _ -> ()
  in

  Type_inference_hooks_js.set_member_hook (use_hook false);
  Type_inference_hooks_js.set_call_hook (use_hook ());
  Type_inference_hooks_js.set_class_member_decl_hook class_def_hook;
  Type_inference_hooks_js.set_obj_prop_decl_hook obj_def_hook;
  Type_inference_hooks_js.set_export_named_hook export_named_hook;
  Type_inference_hooks_js.set_obj_to_obj_hook obj_to_obj_hook

let set_get_refs_hook potential_refs potential_matching_literals target_name =
  let hook ret _ctxt name loc ty =
    begin if name = target_name then
      (* Replace previous bindings of `loc`. We should always use the result of the last call to
       * the hook for a given location. For details see the comment on the generate_tests function
       * in flow_js.ml *)
      potential_refs := LocMap.add loc ty !potential_refs
    end;
    ret
  in
  let lval_hook cx name loc = function
    (* Treat destructuring as a property access *)
    | Type_inference_hooks_js.Parent ty -> hook () cx name loc ty
    | _ -> ()
  in
  let obj_to_obj_hook _ctxt obj1 obj2 =
    let open Type in
    match get_object_literal_loc obj1, obj2 with
    | Some loc, DefT (_, ObjT _) ->
      let entry = (loc, obj2) in
      potential_matching_literals := entry:: !potential_matching_literals
    | _ -> ()
  in

  Type_inference_hooks_js.set_member_hook (hook false);
  Type_inference_hooks_js.set_call_hook (hook ());
  Type_inference_hooks_js.set_lval_hook (lval_hook);
  Type_inference_hooks_js.set_obj_to_obj_hook obj_to_obj_hook

let unset_hooks () =
  Type_inference_hooks_js.reset_hooks ()

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

let all_locs_of_property_def_info def_info =
  def_info
  |> Nel.map loc_of_single_def_info

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
  locs |> Nel.to_list |> List.map Loc.to_string |> String.concat ", "

(* Disable the unused value warning -- we want to keep this around for debugging *)
[@@@warning "-32"]
let debug_string_of_single_def_info = function
  | Class loc -> spf "Class (%s)" (Loc.to_string loc)
  | Object loc -> spf "Object (%s)" (Loc.to_string loc)

let debug_string_of_property_def_info def_info =
  def_info
  |> Nel.map debug_string_of_single_def_info
  |> Nel.to_list
  |> String.concat ", "
  |> spf "[%s]"

let debug_string_of_def_info = function
  | Property (def_info, name) ->
    spf "Property (%s, %s)" (debug_string_of_property_def_info def_info) name
  | CJSExport loc ->
    spf "CJSExport (%s)" (Loc.to_string loc)

let rec debug_string_of_def_loc = function
  | FoundClass locs -> spf "FoundClass (%s)" (debug_string_of_locs locs)
  | FoundObject loc -> spf "FoundObject (%s)" (Loc.to_string loc)
  | FoundUnion def_locs ->
    Nel.to_list def_locs |> List.map debug_string_of_def_loc |> String.concat ", "
    |> spf "FoundUnion (%s)"
  | NoDefFound -> "NoDefFound"
  | UnsupportedType -> "UnsupportedType"
  | AnyType -> "AnyType"
(* Re-enable the unused value warning *)
[@@@warning "+32"]

let extract_instancet cx ty : (Type.t, string) result =
  let open Type in
  let resolved = Flow_js.resolve_type cx ty in
  match resolved with
    | ThisClassT (_, t)
    | DefT (_, PolyT (_, ThisClassT (_, t), _)) -> Ok t
    | _ ->
      let type_string = string_of_ctor resolved in
      Error ("Expected a class type to extract an instance type from, got " ^ type_string)

(* Must be called with the result from Flow_js.Members.extract_type *)
let get_def_loc_from_extracted_type cx extracted_type name =
  extracted_type
  |> Flow_js.Members.extract_members cx
  |> Flow_js.Members.to_command_result
  >>| fun map -> match SMap.get name map with
    | None -> None
    (* Currently some types (e.g. spreads) do not contain locations for their properties. For now
     * we'll just treat them as if the properties do not exist, but once this is fixed this case
     * should be promoted to an error *)
    | Some (None, _) -> None
    | Some (Some loc, _) -> Some loc

let rec extract_def_loc cx ty name : (def_loc, string) result =
  let resolved = Flow_js.resolve_type cx ty in
  extract_def_loc_resolved cx resolved name

(* The same as get_def_loc_from_extracted_type except it recursively checks for overridden
 * definitions of the member in superclasses and returns those as well *)
and extract_def_loc_from_instancet cx extracted_type super name : (def_loc, string) result =
  let current_class_def_loc = get_def_loc_from_extracted_type cx extracted_type name in
  current_class_def_loc
  >>= begin function
    | None -> Ok NoDefFound
    | Some loc ->
      extract_def_loc cx super name
      >>= begin function
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
        | NoDefFound | UnsupportedType | AnyType -> Ok (FoundClass (Nel.one loc))
      end
  end

and extract_def_loc_resolved cx ty name : (def_loc, string) result =
  let open Flow_js.Members in
  let open Type in
  match Flow_js.Members.extract_type cx ty with
    | Success (DefT (_, InstanceT (_, super, _, _))) as extracted_type ->
        extract_def_loc_from_instancet cx extracted_type super name
    | Success (DefT (_, ObjT _)) | SuccessModule _ as extracted_type ->
        get_def_loc_from_extracted_type cx extracted_type name
        >>| begin function
          | None -> NoDefFound
          | Some loc -> FoundObject loc
        end
    | Success (DefT (_, UnionT rep)) ->
        let union_members =
          UnionRep.members rep
          |> List.map (fun member -> extract_def_loc cx member name)
          |> Result.all
        in
        union_members
        >>= begin fun members ->
          Nel.of_list members
          |> Result.of_option ~error:"Union should have at least one member"
        end
        >>| begin fun members_nel ->
          FoundUnion members_nel
        end
    | Success _
    | FailureNullishType
    | FailureUnhandledType _ ->
        Ok UnsupportedType
    | FailureAnyType ->
        Ok AnyType

(* Returns `true` iff the given type is a reference to the symbol we are interested in *)
let type_matches_locs cx ty prop_def_info name =
  let rec def_loc_matches_locs = function
  | FoundClass ty_def_locs ->
    prop_def_info |> Nel.exists begin function
      | Object _ -> false
      | Class loc ->
        (* Only take the first extracted def loc -- that is, the one for the actual definition
         * and not overridden implementations, and compare it to the list of def locs we are
         * interested in *)
        loc = Nel.hd ty_def_locs
    end
  | FoundObject loc ->
    prop_def_info |> Nel.exists begin function
    | Class _ -> false
    | Object def_loc -> loc = def_loc
    end
  | FoundUnion def_locs ->
    def_locs
    |> Nel.map def_loc_matches_locs
    |> Nel.fold_left ( || ) false
  (* TODO we may want to surface AnyType results somehow since we can't be sure whether they
   * are references or not. For now we'll leave them out. *)
  | NoDefFound | UnsupportedType | AnyType -> false
  in
  extract_def_loc cx ty name >>| def_loc_matches_locs

(* Takes the file key where the module reference appeared, as well as the module reference, and
 * returns the file name for the module that the module reference refers to. *)
let file_key_of_module_ref file_key module_ref =
  let resolved = Module_js.find_resolved_module
    ~audit:Expensive.warn
    file_key
    module_ref
  in
  Module_heaps.get_file ~audit:Expensive.warn resolved

let process_prop_refs cx potential_refs file_key prop_def_info name =
  potential_refs |>
    LocMap.bindings |>
    List.map begin fun (ref_loc, ty) ->
      type_matches_locs cx ty prop_def_info name
      >>| function
      | true -> Some ref_loc
      | false -> None
    end
    |> Result.all
    |> Result.map_error ~f:(fun err ->
        Printf.sprintf
          "Encountered while finding refs in `%s`: %s"
          (File_key.to_string file_key)
          err
      )
    >>| begin fun refs ->
      refs
      |> ListUtils.cat_maybes
      |> add_ref_kind FindRefsTypes.PropertyAccess
    end

let property_find_refs_in_file options ast_info file_key def_info name =
  let potential_refs: Type.t LocMap.t ref = ref LocMap.empty in
  let potential_matching_literals: (Loc.t * Type.t) list ref = ref [] in
  let (ast, file_sig, info) = ast_info in
  let info = Docblock.set_flow_mode_for_ide_command info in
  let local_defs =
    Nel.to_list (all_locs_of_property_def_info def_info)
    |> List.filter (fun loc -> loc.Loc.source = Some file_key)
    |> add_ref_kind FindRefsTypes.PropertyDefinition
  in
  let has_symbol = PropertyAccessSearcher.search name ast in
  if not has_symbol then
    Ok local_defs
  else begin
    set_get_refs_hook potential_refs potential_matching_literals name;
    let cx = Merge_service.merge_contents_context
      options file_key ast info file_sig
    in
    unset_hooks ();
    let literal_prop_refs_result =
      (* Lazy to avoid this computation if there are no potentially-relevant object literals to
       * examine *)
      let prop_loc_map = lazy (LiteralToPropLoc.make ast name) in
      let get_prop_loc_if_relevant (obj_loc, into_type) =
        type_matches_locs cx into_type def_info name
        >>| function
        | false -> None
        | true -> LocMap.get obj_loc (Lazy.force prop_loc_map)
      in
      !potential_matching_literals
      |> List.map get_prop_loc_if_relevant
      |> Result.all
      >>| begin fun refs ->
        refs
        |> ListUtils.cat_maybes
        |> add_ref_kind FindRefsTypes.PropertyDefinition
      end
    in
    literal_prop_refs_result
    >>= begin fun literal_prop_refs_result ->
      process_prop_refs cx !potential_refs file_key def_info name
      >>| (@) local_defs
      >>| (@) literal_prop_refs_result
    end
  end

let export_find_refs_in_file ast_info file_key def_loc =
  let open File_sig in
  let (_, file_sig, _) = ast_info in
  let is_relevant module_ref =
    Loc.source def_loc = file_key_of_module_ref file_key module_ref
  in
  let locs = List.fold_left begin fun acc require ->
    match require with
    | Require { source = (_, module_ref); require_loc; _ } ->
      if is_relevant module_ref then
        require_loc::acc
      else
        acc
    | _ -> acc
  end [] file_sig.module_sig.requires in
  let locs =
    if Loc.source def_loc = Some file_key then
      def_loc::locs
    else
      locs
  in
  Ok locs

let add_related_bindings ast_info refs =
  let (ast, file_sig, _) = ast_info in
  let locs = List.map snd refs in
  let related_bindings = ImportExportSymbols.find_related_symbols file_sig locs in
  List.fold_left begin fun acc loc ->
    let new_refs =
      VariableFindRefs.local_find_refs ast loc
      |> Option.value_map ~default:[] ~f:(fun ((_, refs), _) -> refs)
    in
    List.rev_append new_refs acc
  end refs related_bindings

let find_refs_in_file options ast_info file_key def_info =
  let refs = match def_info with
  | Property (def_info, name) ->
    property_find_refs_in_file options ast_info file_key def_info name
  | CJSExport loc ->
    export_find_refs_in_file ast_info file_key loc >>| fun refs ->
    add_ref_kind FindRefsTypes.Other refs
  in
  refs >>| add_related_bindings ast_info

let find_refs_in_multiple_files genv all_deps def_info =
  let {options; workers} = genv in
  let dep_list: File_key.t list = FilenameSet.elements all_deps in
  let node_modules_containers = !Files.node_modules_containers in
  let%lwt result = MultiWorkerLwt.call workers
    ~job: begin fun _acc deps ->
      (* Yay for global mutable state *)
      Files.node_modules_containers := node_modules_containers;
      deps |> List.map begin fun dep ->
        get_ast_result dep >>= fun ast_info ->
        find_refs_in_file options ast_info dep def_info
      end
    end
    ~merge: (fun refs acc -> List.rev_append refs acc)
    ~neutral: []
    ~next: (MultiWorkerLwt.next workers dep_list)
  in
  (* The types got a little too complicated here. Writing out the intermediate types makes it a
   * bit clearer. *)
  let result: (FindRefsTypes.single_ref list list, string) Result.t = Result.all result in
  let result: (FindRefsTypes.single_ref list, string) Result.t = result >>| List.concat in
  Lwt.return result

(* Get the source for each loc. Error if any loc is missing a source. *)
let files_of_locs (locs: Loc.t Nel.t) : (FilenameSet.t, string) result =
  let files_result =
    locs
    |> Nel.map (fun loc -> loc.Loc.source)
    |> Nel.map (Result.of_option ~error:"Expected a location with a source file")
    |> Nel.result_all
  in
  files_result >>| fun files ->
  Nel.to_list files |> FilenameSet.of_list

(* Error if the set is empty *)
let nel_of_filename_set (set: FilenameSet.t) : (File_key.t Nel.t, string) result =
  set
  |> FilenameSet.elements
  |> Nel.of_list
  |> Result.of_option ~error:"Expected a nonempty filename set"

(* Returns the file(s) at which we should begin looking downstream for references. *)
let roots_of_def_info def_info : (File_key.t Nel.t, string) result =
  let root_locs = all_locs_of_def_info def_info in
  files_of_locs root_locs >>= nel_of_filename_set

let deps_of_file_key genv env (file_key: File_key.t) : (FilenameSet.t, string) result Lwt.t =
  let {options; workers} = genv in
  File_key.to_path file_key %>>= fun path ->
  let fileinput = File_input.FileName path in
  File_input.content_of_file_input fileinput %>>| fun content ->
  let%lwt all_deps, _ = get_dependents options workers env file_key content in
  Lwt.return all_deps

let deps_of_file_keys genv env (file_keys: File_key.t list) : (FilenameSet.t, string) result Lwt.t =
  (* We need to use map_s (rather than map_p) because we cannot interleave calls into
   * MultiWorkers. *)
  let%lwt deps_result = Lwt_list.map_s (deps_of_file_key genv env) file_keys in
  Result.all deps_result %>>| fun (deps: FilenameSet.t list) ->
  Lwt.return @@ List.fold_left FilenameSet.union FilenameSet.empty deps

let focus_and_check genv env paths =
  let%lwt new_env, _ =
    Lazy_mode_utils.focus_and_check genv !env paths
  in
  env := new_env;
  Lwt.return_unit

let focus_and_check_filename_set genv env files =
  let paths =
    files
    |> FilenameSet.elements
    |> List.map File_key.to_path
    |> Result.all
  in
  paths %>>| fun paths ->
  Nel.of_list paths
  |> Option.value_map ~default:Lwt.return_unit ~f:(focus_and_check genv env)

(* Returns location pairs such that:
 * - Each location is the definition location for a property with the given
 *   name.
 * - Each pair indicates that the definition locations it contains are related.
 * - For two definition locations to be related, their enclosing object types
 *   must be related.
 * - Two object types are considered related when, in the course of typechecking
 *   the given file, we evaluate an `ObjT ~> ObjT` constraint relating the two
 *   object types.
 * - Note that this can return locations outside of the given file.
*)
let find_related_defs_in_file options name file =
  let get_single_def_info_pairs_if_relevant cx (t1, t2) =
    map2 (extract_def_loc cx t1 name) (extract_def_loc cx t2 name) ~f:begin fun x y -> match x, y with
    | FoundObject loc1, FoundObject loc2 -> [Object loc1, Object loc2]
    | FoundClass class_locs, FoundObject obj_loc ->
      class_locs
      |> Nel.to_list
      |> List.map (fun class_loc -> (Class class_loc, Object obj_loc))
    | _ -> []
    (* TODO union types *)
    end
  in
  let related_types: (Type.t * Type.t) list ref = ref [] in
  let hook _cx t1 t2 =
    related_types := (t1, t2)::!related_types
  in
  Type_inference_hooks_js.set_obj_to_obj_hook hook;
  Type_inference_hooks_js.set_instance_to_obj_hook hook;
  let cx_result =
    get_ast_result file >>| fun (ast, file_sig, docblock) ->
    Merge_service.merge_contents_context
      options file ast docblock file_sig
  in
  unset_hooks ();
  cx_result >>= fun cx ->
  let results: (((single_def_info * single_def_info) list) list, string) result =
    !related_types
    |> List.map (get_single_def_info_pairs_if_relevant cx)
    |> Result.all
  in
  results >>| List.concat

(* Returns all locations which are considered related to the given definition locations. Definition
 * locations are considered related if they refer to a property with the same name, and their
 * enclosing object types appear in a subtype relationship with each other. *)
let find_related_defs
    genv
    env
    (def_info: property_def_info)
    (name: string)
    : (property_def_info, string) result Lwt.t =
  (* Outline:
   * - Create a disjoint set for definition locations
   * - Seed it with every given def_loc
   * - In all files we need to inspect:
   *   - Look for cases where both ObjTs in an ObjT ~> ObjT constraint have a
   *     property with the name we are looking for.
   *   - Add the definition locations of those properties to the disjoint set
   * - Look in the set of related definition locations that we are interested in:
   *   - If any has a source file we have not yet inspected, inspect that file
   *     and all its dependents (less those we have already inspected) as
   *     described above.
   *   - Iterate until we reach a fixed point
   *)
  let {options; workers} = genv in
  let related_defs =
    let uf = UnionFind.of_list (Nel.to_list def_info) in
    let hd, tl = def_info in
    List.iter (UnionFind.union uf hd) tl;
    uf
  in
  let process_files file_set =
    let node_modules_containers = !Files.node_modules_containers in
    let%lwt (result: ((single_def_info * single_def_info) list, string) result list) =
      MultiWorkerLwt.call workers
        ~job: begin fun _acc files ->
          Files.node_modules_containers := node_modules_containers;
          List.map (find_related_defs_in_file options name) files
        end
        ~merge: List.rev_append
        ~neutral: []
        ~next: (MultiWorkerLwt.next workers (FilenameSet.elements file_set))
    in
    Result.all result %>>| fun (pairs: (single_def_info * single_def_info) list list) ->
    List.iter (List.iter (fun (x, y) -> UnionFind.union related_defs x y)) pairs;
    Lwt.return_unit
  in
  let get_unchecked_roots current_def_info checked_files =
    current_def_info |> all_locs_of_property_def_info |> files_of_locs >>| fun roots ->
    FilenameSet.diff roots checked_files
  in
  let get_files_to_check unchecked_roots checked_files =
    let%lwt deps = deps_of_file_keys genv env (FilenameSet.elements unchecked_roots) in
    deps %>>| fun deps ->
    Lwt.return (
      FilenameSet.union
        (FilenameSet.diff deps checked_files)
        unchecked_roots
    )
  in
  let rec loop current_def_info checked_files =
    get_unchecked_roots current_def_info checked_files %>>= fun unchecked_roots ->
    if FilenameSet.is_empty unchecked_roots then
      Lwt.return (Ok current_def_info)
    else begin
      let%lwt result = focus_and_check_filename_set genv env unchecked_roots in
      result %>>= fun () ->
      let%lwt files_to_check = get_files_to_check unchecked_roots checked_files in
      files_to_check %>>= fun files_to_check ->
      let%lwt check_result = process_files files_to_check in
      check_result %>>= fun () ->
      let checked_files = FilenameSet.union checked_files files_to_check in
      let current_def_info =
        let updated_def_info = UnionFind.members related_defs (Nel.hd current_def_info) in
        Nel.of_list updated_def_info
        |> Result.of_option ~error:"Unexpected empty list"
      in
      current_def_info %>>= fun current_def_info ->
      loop current_def_info checked_files
    end
  in
  loop def_info FilenameSet.empty

let def_info_of_typecheck_results cx props_access_info =
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
      def_locs
      |> Nel.map def_info_of_def_loc
      |> Nel.cat_maybes
      |> Option.map ~f:Nel.concat
    | NoDefFound
    | UnsupportedType
    | AnyType -> None
    in
    extract_def_loc cx ty name >>| def_info_of_def_loc
  in
  match props_access_info with
    | None -> Ok None
    | Some (Obj_def (loc, name)) ->
        Ok (Some (Nel.one (Object loc), name))
    | Some (Class_def (ty, name, static)) ->
        if static then
          (* Here, `ty` ends up resolving to `ObjT` so we lose the knowledge that this is a static
           * property. This means that we don't get the fancy look-up-the-inheritance-chain behavior
           * that we get with class instances. That would be nice to add at some point. *)
          def_info_of_type name ty
          >>| Option.map ~f:(fun def_info -> (def_info, name))
        else
          (* We get the type of the class back here, so we need to extract the type of an instance *)
          extract_instancet cx ty >>= fun ty ->
          begin extract_def_loc_resolved cx ty name >>= function
            | FoundClass locs -> Ok (Some (def_info_of_class_member_locs locs, name))
            | FoundUnion _
            | FoundObject _ -> Error "Expected to extract class def info from a class"
            | _ -> Error "Unexpectedly failed to extract definition from known type"
          end
    | Some (Use (ty, name)) ->
        def_info_of_type name ty
        >>| Option.map ~f:(fun def_info -> (def_info, name))
    | Some (Use_in_literal (types, name)) ->
        let def_infos_result =
          Nel.map (def_info_of_type name) types
          |> Nel.result_all
        in
        def_infos_result >>| fun def_infos ->
        Nel.cat_maybes def_infos
        |> Option.map ~f:(Nel.concat)
        |> Option.map ~f:(fun def_info -> (def_info, name))

let add_literal_properties literal_key_info def_info =
  (* If we happen to be on an object property, include the location of that
   * property as a def loc. We don't want to do that above because:
   * (a) We could also encounter a `Use_in_literal` if this object literal flows
   * into another object type. This would force us to make props_access_info a
   * list and add additional complexity just for the sake of this one case.
   * (b) We would have to add a type inference hook, which we are trying to
   * avoid. *)
  let def_info = match def_info, literal_key_info with
  | None, None -> Ok None
  | Some _, None -> Ok def_info
  | None, Some (_, loc, name) -> Ok (Some (Nel.one (Object loc), name))
  | Some (defs, name1), Some (_, loc, name2) ->
    if name1 <> name2 then
      Error "Unexpected name mismatch"
    else
      Ok (Some (Nel.cons (Object loc) defs, name1))
  in
  Result.map def_info ~f:(Option.map ~f:(fun (prop_def_info, name) -> Property (prop_def_info, name)))

let get_def_info genv env profiling file_key content loc: (def_info option, string) result Lwt.t =
  let {options; workers} = genv in
  let props_access_info = ref (Ok None) in
  compute_ast_result file_key content
  %>>= fun (ast, file_sig, info) ->
  let info = Docblock.set_flow_mode_for_ide_command info in
  let literal_key_info: (Loc.t * Loc.t * string) option = ObjectKeyAtLoc.get ast loc in
  let%lwt cx =
    set_def_loc_hook props_access_info literal_key_info loc;
    let%lwt cx = Profiling_js.with_timer_lwt profiling ~timer:"MergeContents" ~f:(fun () ->
      let%lwt () =
        Types_js.ensure_checked_dependencies ~options ~profiling ~workers ~env file_key file_sig
      in
      Lwt.return @@
        Merge_service.merge_contents_context options file_key ast info file_sig
    ) in
    Lwt.return cx
  in
  unset_hooks ();
  !props_access_info %>>= fun props_access_info ->
  let def_info = def_info_of_typecheck_results cx props_access_info in
  let def_info = def_info >>= add_literal_properties literal_key_info in
  let def_info = def_info >>= function
  | Some _ as def_info -> Ok def_info
  | None ->
    (* Check if we are on a CJS import/export. These cases are not covered above since the type
     * system hooks don't quite get us what we want. *)
    let export_loc =
      let open File_sig in
      List.fold_left begin fun acc -> function
      | Require { source = (_, module_ref); require_loc; _ } ->
        if Loc.contains require_loc loc then begin match acc with
        | Error _ -> acc
        | Ok (Some _) -> Error "Did not expect multiple requires to match one location"
        | Ok None ->
          let external_file_sig =
            let filename = file_key_of_module_ref file_key module_ref in
            Option.bind filename Parsing_heaps.get_file_sig
          in
          Result.return @@ Option.bind external_file_sig begin fun external_file_sig ->
            match external_file_sig.module_sig.module_kind with
            | CommonJS { mod_exp_loc=Some loc; _ } -> Some loc
            | _ -> None
          end
        end else acc
      | _ -> acc
      end (Ok None) file_sig.module_sig.requires
    in
    let export_loc = export_loc >>| function
    | Some _ as x -> x
    | None ->
      let open File_sig in
      match file_sig.module_sig.module_kind with
      | CommonJS { mod_exp_loc=Some mod_exp_loc; _ } ->
        if Loc.contains mod_exp_loc loc then Some mod_exp_loc
        else None
      | _ -> None
    in
    Result.map export_loc ~f:(Option.map ~f:(fun x -> CJSExport x))
  in
  Lwt.return @@ def_info

let find_refs_global genv env multi_hop def_info =
  let%lwt def_info =
    if multi_hop then
      match def_info with
      | Property (property_def_info, name) ->
        let%lwt result = find_related_defs genv env property_def_info name in
        result %>>| fun x -> Lwt.return @@ Property (x, name)
      | CJSExport _ -> Lwt.return (Ok def_info)
    else
      Lwt.return (Ok def_info)
  in
  def_info %>>= fun def_info ->
  roots_of_def_info def_info %>>= fun root_file_keys ->
  let root_file_paths_result =
    Nel.map File_key.to_path root_file_keys
    |> Nel.result_all
  in
  root_file_paths_result %>>= fun root_file_paths ->
  let%lwt () = focus_and_check genv env root_file_paths in
  let%lwt deps_result = deps_of_file_keys genv env (Nel.to_list root_file_keys) in
  deps_result %>>= fun deps ->
  let dependent_file_count = FilenameSet.cardinal deps in
  let relevant_files =
    Nel.to_list root_file_keys
    |> FilenameSet.of_list
    |> FilenameSet.union deps
  in
  Hh_logger.info
    "find-refs: searching %d dependent modules for references"
    dependent_file_count;
  let%lwt refs = find_refs_in_multiple_files genv relevant_files def_info in
  refs %>>| fun refs ->
  Lwt.return @@ Some ((display_name_of_def_info def_info, refs), Some dependent_file_count)

let find_refs_local genv file_key content def_info =
  compute_ast_result file_key content >>= fun ast_info ->
  find_refs_in_file genv.options ast_info file_key def_info >>= fun refs ->
  Ok (Some ((display_name_of_def_info def_info, refs), None))

let find_refs genv env ~profiling ~content file_key loc ~global ~multi_hop =
  let%lwt def_info = get_def_info genv env profiling file_key content loc in
  def_info %>>= fun def_info_opt ->
  match def_info_opt with
    | None -> Lwt.return (Ok None)
    | Some def_info ->
        if global || multi_hop then
          find_refs_global genv env multi_hop def_info
        else
          Lwt.return @@ find_refs_local genv file_key content def_info
