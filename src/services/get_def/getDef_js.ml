(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base.Result
open Utils_js
open Parsing_heaps_utils

module Get_def_result = struct
  type t =
    (* the final location of the definition *)
    | Def of Loc.t
    (* if an intermediate get-def failed, return partial progress and the error message *)
    | Partial of Loc.t * string
    (* the input loc didn't point at anything you can call get-def on *)
    | Bad_loc
    (* an unexpected, internal error *)
    | Def_error of string
end

open Get_def_result

(* The result of a get-def is either a final location or an intermediate
   location that is fed into a subsequent get-def. *)
type result =
  | Done of Get_def_result.t
  | Chain of Get_def_request.t

let extract_member_def ~reader cx this name =
  let result =
    match Members.extract cx this |> Members.to_command_result with
    | Ok result_map ->
      (match SMap.find_opt name result_map with
      | Some (None, t) -> Def (Type.loc_of_t t |> loc_of_aloc ~reader)
      | Some (Some x, _) -> Def (loc_of_aloc ~reader x)
      | None -> Def_error (spf "failed to find member %s in members map" name))
    | Error msg -> Def_error msg
  in
  Done result

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
      let ((aloc, v), _) = Base.Option.value ~default:remote local in
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
                  arguments =
                    (_, { ArgList.arguments = Expression ((source_loc, _), _) :: _; comments = _ });
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
        raise (SpecialCase (Done (Def (loc_of_aloc ~reader stmt_loc))))
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
        raise (SpecialCase (Done (Def (loc_of_aloc ~reader stmt_loc))))
      | _ -> super#statement stmt
  end

let resolve_special_cases ~reader ~is_legit_require ~typed_ast def_loc =
  try
    Pervasives.ignore ((new special_caser reader is_legit_require def_loc)#program typed_ast);
    Done (Def def_loc)
  with SpecialCase res -> res

let getdef_from_typed_ast ~options ~reader ~cx ~is_legit_require ~typed_ast = function
  | Get_def_request.Location loc ->
    begin
      match Get_def_process_location.process_location ~is_legit_require ~typed_ast loc with
      | Get_def_process_location.Loc aloc -> Done (Def (loc_of_aloc ~reader aloc))
      | Get_def_process_location.Chain req -> Chain req
      | Get_def_process_location.No_loc -> Done Bad_loc
    end
  | Get_def_request.Identifier { name = _; loc = aloc; type_ } ->
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
        | [] -> Chain (Get_def_request.Type type_)
        | _ :: _ :: _ -> Done (Def_error "Scope builder found multiple matching identifiers")
      end)
  (*
    NOTE:
    The Member, Type, and Require cases could take us to a different file.
    Since none of these will ever return Chain, we do not need to call
    type_contents again to get info about the new file.
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
    let loc =
      match v with
      | Type.OpenT _ ->
        (match Flow_js.possible_types_of_type cx v with
        | [t] -> Def (Type.def_loc_of_t t |> loc_of_aloc ~reader)
        | [] -> Def_error "No possible types"
        | _ -> Def_error "More than one possible type")
      | _ -> Def (Type.def_loc_of_t v |> loc_of_aloc ~reader)
    in
    Done loc
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
      | Some file -> Def Loc.{ none with source = Some file }
      | None -> Def_error (spf "Failed to find imported file %s" name)
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
            Def loc
        | AnyT _ -> get_imported_file ()
        | _ ->
          Def_error
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

let get_def ~options ~reader cx file_sig typed_ast requested_loc =
  let require_loc_map = File_sig.With_Loc.(require_loc_map file_sig.module_sig) in
  let is_legit_require source_aloc =
    let source_loc = loc_of_aloc ~reader source_aloc in
    SMap.exists (fun _ locs -> Nel.exists (fun loc -> loc = source_loc) locs) require_loc_map
  in
  let rec loop rev_req_history req =
    match getdef_from_typed_ast ~options ~reader ~cx ~is_legit_require ~typed_ast req with
    | Done res ->
      let request_history = Base.List.rev_map ~f:gdr_to_string rev_req_history in
      let res =
        match res with
        | Def_error msg ->
          (match recover_intermediate_result rev_req_history with
          | Some recovered_loc -> Partial (recovered_loc, msg)
          | None -> res)
        | _ -> res
      in
      (res, request_history)
    | Chain req -> loop (req :: rev_req_history) req
  in
  let initial_request = Get_def_request.Location requested_loc in
  loop [initial_request] initial_request
