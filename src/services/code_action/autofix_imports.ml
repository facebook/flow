(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast

type named_binding = {
  remote_name: string;
  local_name: string option;
}

type bindings =
  | Default of string
  | Named of named_binding list
  | NamedType of named_binding list
  | Namespace of string

type change =
  | Above of { skip_line: bool }
  | Below of { skip_line: bool }
  | Replace

module ImportKind = struct
  type t =
    | ImportType
    | ImportValue
    | Require

  let compare a b =
    match (a, b) with
    | (ImportType, ImportType) -> 0
    | (ImportValue, ImportValue) -> 0
    | (Require, Require) -> 0
    | (ImportType, _) -> (* types come first *) -1
    | (_, ImportType) -> (* types come first *) 1
    | (_, Require) -> (* requires come last *) -1
    | (Require, _) -> (* requires come last *) 1

  let of_statement = function
    | (_, Statement.ImportDeclaration { Statement.ImportDeclaration.import_kind; _ }) ->
      (match import_kind with
      | Statement.ImportDeclaration.ImportType
      | Statement.ImportDeclaration.ImportTypeof ->
        ImportType
      | Statement.ImportDeclaration.ImportValue -> ImportValue)
    | _ ->
      (* TODO: handle requires *)
      Require
end

module ImportSource = struct
  let compare a b =
    (* TODO: sort global modules above ../ above ./ *)
    String.compare a b

  let of_statement = function
    | ( _,
        Statement.ImportDeclaration
          { Statement.ImportDeclaration.source = (_, { StringLiteral.value; _ }); _ } ) ->
      value
    | _ ->
      (* TODO: handle requires *)
      failwith "only imports are handled so far"
end

let mk_default_import ?loc ?comments ~from name =
  let open Ast_builder in
  let default = Some (Identifiers.identifier name) in
  Statements.import_declaration
    ?loc
    ~import_kind:Statement.ImportDeclaration.ImportValue
    ~source:(Loc.none, string_literal from)
    ~default
    ~specifiers:None
    ?comments

let mk_named_import ?loc ?comments ~import_kind ~from names =
  let open Ast_builder in
  let specifiers =
    let specifiers =
      List.map
        (fun { remote_name; local_name } ->
          let remote = Identifiers.identifier remote_name in
          match local_name with
          | None -> Statements.named_import_specifier remote
          | Some name ->
            Statements.named_import_specifier ~local:(Identifiers.identifier name) remote)
        names
    in
    Some (Statement.ImportDeclaration.ImportNamedSpecifiers specifiers)
  in
  Statements.import_declaration
    ?loc
    ~import_kind
    ~source:(Loc.none, string_literal from)
    ~default:None
    ~specifiers
    ?comments

let mk_namespace_import ?loc ?comments ~from name =
  let open Ast_builder in
  let specifiers =
    let id = Identifiers.identifier name in
    Some (Statement.ImportDeclaration.ImportNamespaceSpecifier (Loc.none, id))
  in
  Statements.import_declaration
    ?loc
    ~import_kind:Statement.ImportDeclaration.ImportValue
    ~source:(Loc.none, string_literal from)
    ~default:None
    ~specifiers
    ?comments

let mk_import ~bindings ~from =
  match bindings with
  | Default id_name -> mk_default_import ~from id_name
  | Named id_names ->
    mk_named_import ~import_kind:Statement.ImportDeclaration.ImportValue ~from id_names
  | NamedType id_names ->
    mk_named_import ~import_kind:Statement.ImportDeclaration.ImportType ~from id_names
  | Namespace id_name -> mk_namespace_import ~from id_name

(* TODO: support inserting requires
    let require_call =
      let open Ast_builder.Expressions in
      let args = arg_list [expression (Literals.string from)] in
      call ~args (identifier "require")
    in
    let declarator =
      match bindings with
      | Default id_name ->
        (* TODO: should add .default depending on facebook_module_interop *)
        variable_declarator id_name ~init:require_call
      | Named id_name -> variable_declarator_generic (Patterns.object_ id_name) (Some require_call)
      | Namespace id_name ->
        (* TODO: depends on facebook_module_interop *)
        variable_declarator id_name ~init:require_call
    in
    const_declaration ?loc ?comments [declarator] *)

let string_of_statement ~options stmt : string =
  let layout = Js_layout_generator.statement ~opts:options stmt in
  let src = Pretty_printer.print ~source_maps:None ~skip_endline:true layout in
  Source.contents src

(** [sorted_insert ~cmp item items], where [items] must be sorted, inserts [item]
    to maintain the sort. if [cmp item x = 0] for some existing item [x], then
    [item] comes first. *)
let sorted_insert ~cmp item items =
  let (rev, inserted) =
    Base.List.fold_left
      ~f:(fun (acc, inserted) next ->
        if (not inserted) && cmp item next <= 0 then
          (next :: item :: acc, true)
        else
          (next :: acc, inserted))
      ~init:([], false)
      items
  in
  let rev =
    if inserted then
      rev
    else
      item :: rev
  in
  Base.List.rev rev

(** compares import specifiers by remote name, ignoring kind and local name

    so, given [import { A, type T, C as Z } ...], A < T and C > T. *)
let compare_specifiers a b =
  let open Statement.ImportDeclaration in
  let { remote = (_, { Identifier.name = a_name; comments = _ }); _ } = a in
  let { remote = (_, { Identifier.name = b_name; comments = _ }); _ } = b in
  String.compare a_name b_name

let insert_import ~options ~bindings ~from =
  string_of_statement ~options (mk_import ~bindings ~from)

let update_import ~options ~bindings stmt =
  let open Statement in
  let open Statement.ImportDeclaration in
  let loc =
    Comment_attachment.statement_comment_bounds stmt
    |> Comment_attachment.expand_loc_with_comment_bounds (fst stmt)
  in
  match stmt with
  | (_, ImportDeclaration { import_kind; source; default; specifiers; comments }) ->
    let edit =
      match (bindings, default, specifiers) with
      | (Default bound_name, Some (_, { Identifier.name; _ }), _)
      | ( Namespace bound_name,
          None,
          Some (ImportNamespaceSpecifier (_, (_, { Identifier.name; _ }))) ) ->
        if bound_name = name then
          (* this should never happen, the name is already in scope *)
          (Above { skip_line = false }, "")
        else
          (* an `import Baz from 'foo'` already exists. weird, but insert
             an `import Foo from 'foo'` anyway. (and similar for `import * as Baz ...`) *)
          let new_stmt =
            let (_, { StringLiteral.value = from; _ }) = source in
            insert_import ~options ~bindings ~from
          in
          let change =
            if String.compare bound_name name < 0 then
              Above { skip_line = false }
            else
              Below { skip_line = false }
          in
          (change, new_stmt)
      | (Default _, None, Some _) ->
        (* a `import {bar} from 'foo'` or `import * as Foo from 'foo'` already exists.
           rather than change it to `import Foo, {bar} from 'foo'`, we choose to insert
           a separate import. TODO: maybe make this a config option? *)
        let new_stmt =
          let (_, { StringLiteral.value = from; _ }) = source in
          insert_import ~options ~bindings ~from
        in
        (Above { skip_line = false }, new_stmt)
      | (Named bound_names, _, Some (ImportNamedSpecifiers specifiers))
      | (NamedType bound_names, _, Some (ImportNamedSpecifiers specifiers)) ->
        let open ImportDeclaration in
        let kind =
          match (import_kind, bindings) with
          | (ImportValue, NamedType _) -> Some ImportType
          | _ -> None
        in
        let new_specifiers =
          List.map
            (fun { remote_name; local_name } ->
              {
                kind;
                local = Base.Option.map ~f:Ast_builder.Identifiers.identifier local_name;
                remote = Ast_builder.Identifiers.identifier remote_name;
              })
            bound_names
        in
        let new_specifiers =
          if Base.List.is_sorted ~compare:compare_specifiers specifiers then
            List.fold_left
              (fun specifiers new_specifier ->
                sorted_insert ~cmp:compare_specifiers new_specifier specifiers)
              specifiers
              new_specifiers
          else
            specifiers @ new_specifiers
        in
        let specifiers = Some (ImportNamedSpecifiers new_specifiers) in
        let stmt =
          (loc, ImportDeclaration { import_kind; source; default; specifiers; comments })
        in
        let edit = string_of_statement ~options stmt in
        (Replace, edit)
      | (Named _, Some _, _)
      | (Named _, None, Some (ImportNamespaceSpecifier _))
      | (NamedType _, Some _, _)
      | (NamedType _, None, Some (ImportNamespaceSpecifier _))
      | (Namespace _, Some _, _)
      | (Namespace _, None, Some (ImportNamedSpecifiers _)) ->
        (* trying to insert a named specifier, but a default or namespace import already
           exists. rather than change it to `import Foo, {bar} from 'foo'`, we choose to
           insert a separate import `import {bar} from 'foo'`.
           TODO: maybe make this a config option? *)
        let new_stmt =
          let (_, { StringLiteral.value = from; _ }) = source in
          insert_import ~options ~bindings ~from
        in
        (Below { skip_line = false }, new_stmt)
      | (_, None, None) -> failwith "unexpected import with neither a default nor specifiers"
    in
    (loc, edit)
  | _ -> failwith "trying to update a non-import"

let compare_imports a b =
  let k = ImportKind.(compare (of_statement a) (of_statement b)) in
  if k = 0 then
    ImportSource.(compare (of_statement a) (of_statement b))
  else
    k

(** [sorted_insertion_point import imports] walks [imports], ensuring it is
  sorted, and looking for the correct place to insert [import]. returns [None]
  if [imports] is not sorted, or [Some (loc, change)] where [change] is whether
  to insert above or below. *)
let sorted_insertion_point =
  let potential_change import current =
    let k = ImportKind.(compare (of_statement import) (of_statement current)) in
    if k = 0 then
      (* same section *)
      let k = ImportSource.(compare (of_statement import) (of_statement current)) in
      if k < 0 then
        Above { skip_line = false }
      else
        Below { skip_line = false }
    else if k < 0 then
      (* `current` is in the next section *)
      Above { skip_line = true }
    else
      (* `current` is in the previous section *)
      Below { skip_line = true }
  in
  (* Basically, insertion sort. For each import `current` in `imports`, check
     whether `import` should be inserted `Above` it, in which case we're done,
     or somewhere `Below`. If `Below`, we keep going until we go "too far"
     or run out of imports. *)
  let rec helper acc import imports =
    match imports with
    | [] -> acc
    | current :: rest ->
      let sorted =
        match rest with
        | [] -> true
        | next :: _ -> compare_imports current next <= 0
      in
      if sorted then
        let acc =
          match acc with
          | None
          | Some (_, Below _) ->
            Some (fst current, potential_change import current)
          | Some (_, Above _)
          | Some (_, Replace) ->
            acc
        in
        helper acc import rest
      else
        (* imports are not sorted, so give up *)
        None
  in
  (fun import imports -> helper None import imports)

let rec last_loc = function
  | [] -> failwith "empty list"
  | [(loc, _)] -> loc
  | _ :: rest -> last_loc rest

let insertion_point ~imports ~body import =
  match sorted_insertion_point import imports with
  | Some edit -> edit
  | None ->
    (match (imports, body) with
    | ([], []) -> failwith "trying to fix a missing import in an empty file"
    | (_ :: _, _) ->
      (* below last import *)
      let loc = last_loc imports in
      (loc, Below { skip_line = false })
    | (_, (loc, _) :: _) ->
      (* above first body statement *)
      (loc, Above { skip_line = true }))

let kind_matches_bindings bindings import_kind =
  let open ImportKind in
  (* TODO: confirm CJS/ESM interop, depends on flowconfig *)
  match (import_kind, bindings) with
  | (ImportType, NamedType _) -> true
  | (ImportType, (Default _ | Named _ | Namespace _)) -> false
  | (Require, Default _) -> true
  | (Require, (Named _ | NamedType _ | Namespace _)) -> false
  | (ImportValue, NamedType _) -> false
  | (ImportValue, (Default _ | Named _ | Namespace _)) -> true

let existing_import ~bindings ~from imports =
  let open Statement in
  let potentials =
    Base.List.filter
      ~f:(fun stmt ->
        kind_matches_bindings bindings (ImportKind.of_statement stmt)
        && ImportSource.of_statement stmt = from)
      imports
  in
  let bindings_type_matches ~default ~specifiers =
    match (bindings, default, specifiers) with
    | (Default _, Some _, _) -> true
    | (Named _, _, Some (ImportDeclaration.ImportNamedSpecifiers _)) -> true
    | _ -> false
  in
  let rec closest potentials =
    match potentials with
    | [] -> None
    | [stmt] -> Some stmt
    | stmt :: stmts ->
      (match stmt with
      | (_, ImportDeclaration { ImportDeclaration.default; specifiers; _ })
        when bindings_type_matches ~default ~specifiers ->
        Some stmt
      | _ -> closest stmts)
  in
  closest potentials

let add_single_import ~options ~bindings ~from ~imports ~body : Loc.t * string =
  let (loc, edit) =
    match existing_import ~bindings ~from imports with
    | Some stmt -> update_import ~options ~bindings stmt
    | None ->
      let new_import = mk_import ~bindings ~from in
      let (loc, change) = insertion_point ~imports ~body new_import in
      (loc, (change, string_of_statement ~options new_import))
  in
  let edit =
    match edit with
    | (Above { skip_line }, str) ->
      (* str doesn't include a newline, so we add one. if we want to skip
         a line, we add 2. *)
      let loc = Loc.start_loc loc in
      if skip_line then
        (loc, str ^ "\n\n")
      else
        (loc, str ^ "\n")
    | (Below { skip_line }, str) ->
      (* loc of the previous statement doesn't include the trailing \n,
         so we add one to the beginning to replace it, and the existing
         one ends up trailing our inserted statement. if we want to skip
         a line, we add 2. *)
      let loc = Loc.end_loc loc in
      if skip_line then
        (loc, "\n\n" ^ str)
      else
        (loc, "\n" ^ str)
    | (Replace, str) -> (loc, str)
  in
  edit

let add_import ~options ~bindings ~from ast : (Loc.t * string) list =
  let (Flow_ast_differ.Partitioned { directives = _; imports; body }) =
    let (_, { Program.statements; _ }) = ast in
    Flow_ast_differ.partition_imports statements
  in
  [add_single_import ~options ~bindings ~from ~imports ~body]

let add_imports ~options ~added_imports ast =
  let (Flow_ast_differ.Partitioned { directives = _; imports; body }) =
    let (_, { Program.statements; _ }) = ast in
    Flow_ast_differ.partition_imports statements
  in
  List.map
    (fun (from, bindings) -> add_single_import ~options ~from ~bindings ~imports ~body)
    added_imports

module Identifier_finder = struct
  type kind =
    | Type_identifier
    | Value_identifier

  exception Found of kind

  class mapper target =
    object (this)
      inherit [Loc.t] Flow_ast_contains_mapper.mapper

      method loc_annot_contains_target loc = Loc.contains loc target

      method! identifier id =
        let (loc, _) = id in
        if this#loc_annot_contains_target loc then raise (Found Value_identifier);
        id

      method! type_identifier id =
        let (loc, _) = id in
        if this#loc_annot_contains_target loc then raise (Found Type_identifier);
        id
    end
end

let loc_is_type ~ast loc =
  let mapper = new Identifier_finder.mapper loc in
  try
    let _ast = mapper#program ast in
    false
  with
  | Identifier_finder.(Found Value_identifier) -> false
  | Identifier_finder.(Found Type_identifier) -> true
