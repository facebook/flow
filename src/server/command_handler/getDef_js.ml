(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core_result
open Utils_js
open Parsing_heaps_utils

(* The result of a get-def is either a final location or an intermediate
   location that is fed into a subsequent get-def. *)
type result =
  | Done of Loc.t
  | Chain of Get_def_request.t

let extract_member_def ~reader cx this name =
  let member_result = Members.extract cx this in
  let (result_str, t) =
    Members.(
      match member_result with
      | Success _ -> ("SUCCESS", this)
      | SuccessModule _ -> ("SUCCESS", this)
      | FailureNullishType -> ("FAILURE_NULLABLE", this)
      | FailureAnyType -> ("FAILURE_NO_COVERAGE", this)
      | FailureUnhandledType t -> ("FAILURE_UNHANDLED_TYPE", t)
      | FailureUnhandledMembers t -> ("FAILURE_UNHANDLED_MEMBERS", t))
  in
  let json_data_to_log =
    Hh_json.(
      JSON_Object
        [
          ("type", Debug_js.json_of_t ~depth:3 cx t);
          ("gd_name", JSON_String name);
          ("result", JSON_String result_str);
        ])
  in
  let command_result = Members.to_command_result member_result in
  ( Done
      begin
        match command_result with
        | Error _ -> Loc.none
        | Ok result_map ->
          begin
            match SMap.get name result_map with
            | Some (loc, t) ->
              begin
                match loc with
                | None -> Type.loc_of_t t |> loc_of_aloc ~reader
                | Some x -> loc_of_aloc ~reader x
              end
            | None -> Loc.none
          end
      end,
    Some json_data_to_log )

(* turns typed AST into normal AST so we can run Scope_builder on it *)
(* TODO(vijayramamurthy): make scope builder polymorphic *)
class type_killer (reader : Parsing_heaps.Reader.reader) =
  object
    inherit [ALoc.t, ALoc.t * Type.t, Loc.t, Loc.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot x = loc_of_aloc ~reader x

    method on_type_annot (x, _) = loc_of_aloc ~reader x
  end

exception SpecialCase of result

class special_caser (reader : Parsing_heaps.Reader.reader) (target_loc : Loc.t) =
  object (this)
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    method on_loc_annot x = x

    method on_type_annot x = x

    method covers_target aloc = Loc.contains (loc_of_aloc ~reader aloc) target_loc

    (* Following ES6 imports through to their sources *)
    method! import_default_specifier (((aloc, v), _) as x) =
      if this#covers_target aloc then raise (SpecialCase (Chain (Get_def_request.Type v)));
      super#import_default_specifier x

    method! import_named_specifier
        ({ Flow_ast.Statement.ImportDeclaration.local; remote; kind = _ } as x) =
      let ((aloc, v), _) = Option.value ~default:remote local in
      if this#covers_target aloc then raise (SpecialCase (Chain (Get_def_request.Type v)));
      super#import_named_specifier x

    (* Following "require" imports through to their sources *)
    method! variable_declarator
        ~kind ((_, { Flow_ast.Statement.VariableDeclaration.Declarator.id; init }) as x) =
      let ((id_loc, _), _) = id in
      match init with
      | Some
          Flow_ast.Expression.
            ( _,
              Call
                {
                  Call.callee = (_, Identifier (_, { Flow_ast.Identifier.name = "require"; _ }));
                  _;
                } )
        when this#covers_target id_loc ->
        raise (SpecialCase (Chain (Get_def_request.Location target_loc)))
      | _ -> super#variable_declarator ~kind x

    (* Minor "repositioning" of get_def results *)

    (*
    Scope_api reports the definition of a namespaced import as:
      import * as foo from "bar"
                  ^^^
    But to keep consistent with previous behavior, we instead report it as:
      import * as foo from "bar"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
  *)
    method! import_declaration stmt_loc decl =
      match decl with
      | Flow_ast.Statement.ImportDeclaration.
          { specifiers = Some (ImportNamespaceSpecifier (id_loc, _)); _ }
        when this#covers_target id_loc ->
        raise (SpecialCase (Done (loc_of_aloc ~reader stmt_loc)))
      | _ -> super#import_declaration stmt_loc decl

    (*
    Scope_api reports the definition of a declared function as:
      function foo(bar) { baz }
               ^^^
    But to keep consistent with previous behavior, we instead report it as:
      function foo(bar) { baz }
      ^^^^^^^^^^^^^^^^^^^^^^^^^
  *)
    method! statement ((stmt_loc, stmt') as stmt) =
      match stmt' with
      | Flow_ast.(Statement.FunctionDeclaration { Function.id = Some ((id_loc, _), _); _ })
        when this#covers_target id_loc ->
        raise (SpecialCase (Done (loc_of_aloc ~reader stmt_loc)))
      | _ -> super#statement stmt
  end

let resolve_special_cases reader typed_ast def_loc =
  try
    Pervasives.ignore ((new special_caser reader def_loc)#program typed_ast);
    Done def_loc
  with SpecialCase res -> res

let getdef_from_typed_ast ~options ~reader cx typed_ast = function
  | Get_def_request.Location loc ->
    Option.map (Get_def_process_location.process_location ~typed_ast loc) (fun req ->
        (Chain req, None))
  | Get_def_request.(Member { prop_name = name; object_source }) ->
    let obj_t =
      match object_source with
      | Get_def_request.ObjectType t -> t
      | Get_def_request.ObjectRequireLoc loc -> Context.find_require cx loc
    in
    Some (extract_member_def ~reader cx obj_t name)
  | Get_def_request.Identifier (_, aloc) ->
    let loc = loc_of_aloc ~reader aloc in
    let ast = (new type_killer reader)#program typed_ast in
    let scope_info = Scope_builder.program ast in
    let all_uses = Scope_api.With_Loc.all_uses scope_info in
    Loc_collections.(
      let matching_uses = LocSet.filter (fun use -> Loc.contains use loc) all_uses in
      begin
        match LocSet.elements matching_uses with
        | [use] ->
          let def = Scope_api.With_Loc.def_of_use scope_info use in
          let def_loc = def.Scope_api.With_Loc.Def.locs |> Nel.hd in
          let result = resolve_special_cases reader typed_ast def_loc in
          Some (result, None)
        | [] -> None
        | _ :: _ :: _ -> failwith "Multiple identifiers were unexpectedly matched"
      end)
  | Get_def_request.Type v ->
    begin
      match Flow_js.possible_types_of_type cx v with
      | [t] -> Some (Done (Type.def_loc_of_t t |> loc_of_aloc ~reader), None)
      | _ -> None
    end
  | Get_def_request.Require ((source_loc, name), require_loc) ->
    let module_t = Context.find_require cx source_loc |> Members.resolve_type cx in
    (* function just so we don't do the work unless it's actually needed. *)
    let get_imported_file () =
      let filename =
        Module_heaps.Reader.get_file
          ~reader
          ~audit:Expensive.warn
          (Module_js.imported_module
             ~options
             ~reader:(Abstract_state_reader.State_reader reader)
             ~node_modules_containers:!Files.node_modules_containers
             (Context.file cx)
             (Nel.one require_loc)
             name)
      in
      match filename with
      | Some file -> Loc.{ none with source = Some file }
      | None -> Loc.none
    in
    Some
      ( Done
          Type.(
            match module_t with
            | ModuleT (_, { cjs_export; _ }, _) ->
              (* If we have a location for the cjs export, go there. Otherwise
               * fall back to just the top of the file *)
              let loc =
                match cjs_export with
                | Some t -> loc_of_t t |> loc_of_aloc ~reader (* This can return Loc.none *)
                | None -> Loc.none
              in
              if loc = Loc.none then
                get_imported_file ()
              else
                loc
            | AnyT _ -> get_imported_file ()
            | _ ->
              failwith
                (spf
                   "Internal Flow Error: Expected ModuleT for %S, but got %S!"
                   name
                   (string_of_ctor module_t))),
        None )

let recover_intermediate_loc = function
  | Get_def_request.Location loc -> loc
  | _ -> Loc.none

let rec getdef_get_result ~options ~reader cx typed_ast req =
  match getdef_from_typed_ast ~options ~reader cx typed_ast req with
  | Some (Done loc, json) -> (loc, json)
  | Some (Chain req', json) ->
    let (loc, json') = getdef_get_result ~options ~reader cx typed_ast req' in
    if loc = Loc.none then
      (recover_intermediate_loc req', json)
    else
      (loc, json')
  | None -> (Loc.none, None)

let get_def ~options ~reader ~env ~profiling (file_input, line, col) =
  let filename = File_input.filename_of_file_input file_input in
  let file = File_key.SourceFile filename in
  let loc = Loc.make file line col in
  let%lwt check_result =
    File_input.content_of_file_input file_input
    %>>= (fun content -> Types_js.basic_check_contents ~options ~env ~profiling content file)
  in
  let%lwt getdef_result =
    map_error ~f:(fun str -> (str, None)) check_result
    %>>= fun (cx, _, _, typed_ast) ->
    Profiling_js.with_timer_lwt profiling ~timer:"GetResult" ~f:(fun () ->
        try_with_json (fun () ->
            let (res_loc, res_json) =
              getdef_get_result ~reader ~options cx typed_ast (Get_def_request.Location loc)
            in
            (* If the result covers the requested location then we didn't find anything helpful *)
            if Loc.(res_loc.source = loc.source) && Reason.in_range loc res_loc then
              Lwt.return (Ok (Loc.none, res_json))
            else
              Lwt.return (Ok (res_loc, res_json))))
  in
  Lwt.return (split_result getdef_result)
