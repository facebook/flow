(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Utils_js

let ( >>= ) = Core_result.( >>= )

let ( >>| ) = Core_result.( >>| )

let get_ref_kinds refs loc =
  refs |> List.filter (fun (_, ref_loc) -> ref_loc = loc) |> Core_list.map ~f:fst

class rename_mapper refs new_name =
  object (this)
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    method! identifier (expr : (Loc.t, Loc.t) Ast.Identifier.t) =
      let (loc, _) = expr in
      if List.exists (fun (_, ref_loc) -> ref_loc = loc) refs then
        Flow_ast_utils.ident_of_source (loc, new_name)
      else
        expr

    method! object_property_type (opt : (Loc.t, Loc.t) Ast.Type.Object.Property.t) =
      Ast.Type.Object.Property.(
        let opt = super#object_property_type opt in
        let (loc, ({ key; _ } as property)) = opt in
        let key' =
          Ast.Expression.Object.Property.(
            match key with
            | Identifier id ->
              let id' = this#identifier id in
              if id == id' then
                key
              else
                Identifier id'
            | _ -> key)
        in
        if key == key' then
          opt
        else
          (loc, { property with key = key' }))

    method! pattern_object_property ?kind (prop : (Loc.t, Loc.t) Ast.Pattern.Object.Property.t') =
      Ast.Pattern.Object.Property.(
        let { key; pattern; default; shorthand } = prop in
        if not shorthand then
          super#pattern_object_property prop
        else
          let key_loc =
            match key with
            | Literal (x, _)
            | Identifier (x, _)
            | Computed (x, _) ->
              x
          in
          let ref_kinds = get_ref_kinds refs key_loc in
          let key' =
            if List.mem FindRefsTypes.PropertyAccess ref_kinds then
              this#pattern_object_property_key ?kind key
            else
              key
          in
          let pattern' =
            if List.mem FindRefsTypes.Local ref_kinds then
              this#pattern_object_property_pattern ?kind pattern
            else
              pattern
          in
          (* TODO *)
          let default' = default in
          if key == key' && pattern == pattern' && default == default' then
            prop
          else
            (* TODO if both changed (e.g. destructuring requires) then retain shorthand *)
            { key = key'; pattern = pattern'; default = default'; shorthand = false })

    method! object_property (prop : (Loc.t, Loc.t) Ast.Expression.Object.Property.t) =
      Ast.Expression.Object.Property.(
        match prop with
        | (loc, Init { key; value; shorthand }) ->
          if not shorthand then
            super#object_property prop
          else
            let key_loc =
              match key with
              | Literal (x, _)
              | Identifier (x, _)
              | PrivateName (x, _)
              | Computed (x, _) ->
                x
            in
            let ref_kinds = get_ref_kinds refs key_loc in
            (* What about computed properties? *)
            let key' =
              if List.mem FindRefsTypes.PropertyDefinition ref_kinds then
                this#object_key key
              else
                key
            in
            let value' =
              if List.mem FindRefsTypes.Local ref_kinds then
                this#expression value
              else
                value
            in
            if key == key' && value == value' then
              prop
            else
              (loc, Init { key = key'; value = value'; shorthand = false })
        (* TODO *)
        | _ -> super#object_property prop)
  end

let mapper_to_edits (ast_mapper : Loc.t Flow_ast_mapper.mapper) (ast : (Loc.t, Loc.t) Ast.program)
    =
  let new_ast = ast_mapper#program ast in
  let changes = Flow_ast_differ.program Flow_ast_differ.Standard ast new_ast in
  Ast_diff_printer.edits_of_changes None changes

let get_with_default default key map = FilenameMap.find_opt key map |> Option.value ~default

let split_by_source refs =
  List.fold_left
    begin
      fun acc ref ->
      let (_, loc) = ref in
      acc
      >>= fun map ->
      Core_result.of_option ~error:"No source found" Loc.(loc.source)
      >>= fun source ->
      let lst = ref :: get_with_default [] source map in
      Ok (FilenameMap.add source lst map)
    end
    (Ok FilenameMap.empty)
    refs

let apply_rename_to_file _file ast refs new_name =
  let mapper = new rename_mapper refs new_name in
  mapper_to_edits mapper ast

let apply_rename_to_files ~reader refs_by_file new_name =
  FilenameMap.fold
    begin
      fun file refs acc ->
      acc
      >>= fun edits ->
      FindRefsUtils.get_ast_result ~reader file
      >>| fun (ast, _, _) ->
      let file_edits = apply_rename_to_file file ast refs new_name in
      List.rev_append file_edits edits
    end
    refs_by_file
    (Ok [])
  >>| List.rev

type refactor_result = ((Loc.t * string) list option, string) result Lwt.t

let rename ~reader ~genv ~env ~profiling ~file_input ~line ~col ~new_name =
  (* TODO verify that new name is a valid identifier *)
  (* TODO maybe do something with the json? *)
  (* TODO support rename based on multi-hop find-refs *)
  let%lwt (find_refs_response, _) =
    FindRefs_js.find_global_refs
      ~reader
      ~genv
      ~env
      ~profiling
      ~file_input
      ~line
      ~col
      ~multi_hop:false
  in
  find_refs_response
  %>>= function
  | None -> Lwt.return (Ok None)
  | Some (_old_name, refs) ->
    (* TODO prevent naming conflicts *)
    (* TODO only rename renameable locations (e.g. not `default` in `export default`) *)
    split_by_source refs
    %>>= fun refs_by_file ->
    apply_rename_to_files ~reader refs_by_file new_name
    %>>= (fun (edits : (Loc.t * string) list) -> Lwt.return @@ Ok (Some edits))

let refactor ~reader ~genv ~env ~profiling ~file_input ~line ~col ~refactor_variant :
    refactor_result =
  match refactor_variant with
  | ServerProt.Request.RENAME new_name ->
    rename ~reader ~genv ~env ~profiling ~file_input ~line ~col ~new_name
