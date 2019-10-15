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
  | Done of (Loc.t, string) Pervasives.result
  | Chain of Get_def_request.t

let extract_member_def ~reader cx this name =
  let member_result = Members.extract cx this in
  let command_result = Members.to_command_result member_result in
  Done
    ( command_result
    >>= fun result_map ->
    match SMap.get name result_map with
    | Some (None, t) -> Ok (Type.loc_of_t t |> loc_of_aloc ~reader)
    | Some (Some x, _) -> Ok (loc_of_aloc ~reader x)
    | None -> Error (spf "failed to find member %s in members map" name) )

(* turns typed AST into normal AST so we can run Scope_builder on it *)
(* TODO(vijayramamurthy): make scope builder polymorphic *)
class type_killer (reader : Parsing_heaps.Reader.reader) =
  object
    inherit [ALoc.t, ALoc.t * Type.t, Loc.t, Loc.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot x = loc_of_aloc ~reader x

    method on_type_annot (x, _) = loc_of_aloc ~reader x
  end

exception SpecialCase of result

class special_caser
  (reader : Parsing_heaps.Reader.reader) (is_legit_require : ALoc.t -> bool) (target_loc : Loc.t) =
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
                  arguments = Expression ((source_loc, _), _) :: _;
                  _;
                } )
        when is_legit_require source_loc && this#covers_target id_loc ->
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
        raise (SpecialCase (Done (Ok (loc_of_aloc ~reader stmt_loc))))
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
        raise (SpecialCase (Done (Ok (loc_of_aloc ~reader stmt_loc))))
      | _ -> super#statement stmt
  end

let resolve_special_cases ~reader ~is_legit_require ~typed_ast def_loc =
  try
    Pervasives.ignore ((new special_caser reader is_legit_require def_loc)#program typed_ast);
    Done (Ok def_loc)
  with SpecialCase res -> res

let getdef_from_typed_ast ~options ~reader ~cx ~is_legit_require ~typed_ast = function
  | Get_def_request.Location loc ->
    begin
      match Get_def_process_location.process_location ~is_legit_require ~typed_ast loc with
      | Some req -> Chain req
      | None -> Done (Error "Typed_ast_utils.find_get_def_info failed")
    end
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
          resolve_special_cases ~reader ~is_legit_require ~typed_ast def_loc
        | [] -> Done (Error "Scope builder found no matching identifiers")
        | _ :: _ :: _ -> Done (Error "Scope builder found multiple matching identifiers")
      end)
  (*
    NOTE:
    The Member, Type, and Require cases could take us to a different file.
    Since none of these will ever return Chain, we do not need to call
    basic_check_contents again to get info about the new file.
    If you break this serendipitous invariant, you'll probably need to add logic
    to deal with updating what file we're looking at.
  *)
  | Get_def_request.(Member { prop_name = name; object_source }) ->
    let obj_t =
      match object_source with
      | Get_def_request.ObjectType t -> t
      | Get_def_request.ObjectRequireLoc loc -> Context.find_require cx loc
    in
    extract_member_def ~reader cx obj_t name
  | Get_def_request.Type v ->
    begin
      match Flow_js.possible_types_of_type cx v with
      | [t] -> Done (Ok (Type.def_loc_of_t t |> loc_of_aloc ~reader))
      | _ -> Done (Error "Flow_js.possible_types_of_type failed")
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
      | Some file -> Ok Loc.{ none with source = Some file }
      | None -> Error (spf "Failed to find imported file %s" name)
    in
    Done
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
            Ok loc
        | AnyT _ -> get_imported_file ()
        | _ ->
          Error
            (spf
               "Internal Flow Error: Expected ModuleT for %S, but got %S!"
               name
               (string_of_ctor module_t)))

let rec recover_intermediate_result = function
  | []
  | [_] ->
    (* don't try to recover from the very first request because that was the
      original request so it's not helpful to return it *)
    (* report Loc.none as success in this case just to preserve existing behavior. *)
    Some Loc.none
  | Get_def_request.Location loc :: _ -> Some loc
  | _ :: reqs -> recover_intermediate_result reqs

let gdr_to_string = function
  | Get_def_request.Location _ -> "Location"
  | Get_def_request.Identifier _ -> "Identifier"
  | Get_def_request.Type _ -> "Type"
  | Get_def_request.Member _ -> "Member"
  | Get_def_request.Require _ -> "Require"

let getdef_get_result ~options ~reader ~cx ~file_sig ~typed_ast requested_loc =
  let require_loc_map = File_sig.With_Loc.(require_loc_map file_sig.module_sig) in
  let is_legit_require source_aloc =
    let source_loc = loc_of_aloc ~reader source_aloc in
    SMap.exists (fun _ locs -> Nel.exists (fun loc -> loc = source_loc) locs) require_loc_map
  in
  let rec loop rev_req_history req =
    match getdef_from_typed_ast ~options ~reader ~cx ~is_legit_require ~typed_ast req with
    | Done res ->
      let request_history =
        ( "request_history",
          Hh_json.JSON_Array
            (List.rev_map (fun req -> Hh_json.JSON_String (gdr_to_string req)) rev_req_history) )
      in
      begin
        match res with
        | Ok _ ->
          (res, Hh_json.JSON_Object [request_history; ("result", Hh_json.JSON_String "SUCCESS")])
        | Error msg ->
          ( (match recover_intermediate_result rev_req_history with
            | Some recovered_loc -> Ok recovered_loc
            | None -> res),
            Hh_json.JSON_Object
              [
                ("error", Hh_json.JSON_String msg);
                request_history;
                ("result", Hh_json.JSON_String "FAILURE");
              ] )
      end
    | Chain req -> loop (req :: rev_req_history) req
  in
  let initial_request = Get_def_request.Location requested_loc in
  loop [initial_request] initial_request
  |> Utils_js.map_fst (fun res ->
         res
         >>= Loc.(
               fun result_loc ->
                 if
                   result_loc.source = requested_loc.source
                   && Reason.in_range requested_loc result_loc
                 then
                   return Loc.none
                 else
                   return result_loc))

let get_def ~options ~reader ~env ~profiling (file_input, line, col) =
  let filename = File_input.filename_of_file_input file_input in
  let file = File_key.SourceFile filename in
  let loc = Loc.make file line col in
  let%lwt check_result =
    File_input.content_of_file_input file_input
    %>>= (fun content -> Types_js.basic_check_contents ~options ~env ~profiling content file)
  in
  match check_result with
  | Error msg ->
    Lwt.return (Error msg, Some (Hh_json.JSON_Object [("error", Hh_json.JSON_String msg)]))
  | Ok (cx, _, file_sig, typed_ast) ->
    Profiling_js.with_timer_lwt profiling ~timer:"GetResult" ~f:(fun () ->
        try_with_json2 (fun () ->
            Lwt.return
              ( getdef_get_result ~reader ~options ~cx ~file_sig ~typed_ast loc
              |> (fun (a, b) -> (a, Some b)) )))
