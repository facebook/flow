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

type placement =
  | Above of { skip_line: bool }
  | Below of { skip_line: bool }
  | Replace

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

  let is_lower source =
    String.length source > 1
    &&
    match source.[0] with
    | 'a' .. 'z' -> true
    | _ -> false
end

module Section = struct
  type t =
    | ImportType
    | ImportValueFromRelative  (** import ... from './foo' *)
    | ImportValueFromModule  (** import ... from 'foo' *)
    | Require

  let compare a b =
    match (a, b) with
    | (ImportType, ImportType) -> 0
    | (ImportValueFromRelative, ImportValueFromRelative) -> 0
    | (ImportValueFromModule, ImportValueFromModule) -> 0
    | (Require, Require) -> 0
    | (ImportType, _) -> (* types come first *) -1
    | (_, ImportType) -> (* types come first *) 1
    | (ImportValueFromRelative, ImportValueFromModule) -> -1
    | (ImportValueFromModule, ImportValueFromRelative) -> 1
    | (_, Require) -> (* requires come last *) -1
    | (Require, _) -> (* requires come last *) 1

  let of_statement = function
    | ( _,
        Statement.ImportDeclaration
          { Statement.ImportDeclaration.import_kind; source = (_, { StringLiteral.value; _ }); _ }
      ) ->
      (match import_kind with
      | Statement.ImportDeclaration.ImportType
      | Statement.ImportDeclaration.ImportTypeof ->
        ImportType
      | Statement.ImportDeclaration.ImportValue ->
        if ImportSource.is_lower value then
          ImportValueFromModule
        else
          ImportValueFromRelative)
    | _ ->
      (* TODO: handle requires *)
      Require
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

let update_import ~bindings stmt =
  let open Statement in
  let open Statement.ImportDeclaration in
  let loc =
    Comment_attachment.statement_comment_bounds stmt
    |> Comment_attachment.expand_loc_with_comment_bounds (fst stmt)
  in
  match stmt with
  | (_, ImportDeclaration { import_kind; source; default; specifiers; comments }) ->
    let (_, { StringLiteral.value = from; _ }) = source in
    let edit =
      match (bindings, default, specifiers) with
      | (Default bound_name, Some (_, { Identifier.name; _ }), _)
      | ( Namespace bound_name,
          None,
          Some (ImportNamespaceSpecifier (_, (_, { Identifier.name; _ }))) ) ->
        if bound_name = name then
          (* this should never happen, the name is already in scope *)
          (Replace, stmt)
        else
          (* an `import Baz from 'foo'` already exists. weird, but insert
             an `import Foo from 'foo'` anyway. (and similar for `import * as Baz ...`) *)
          let placement =
            if String.compare bound_name name < 0 then
              Above { skip_line = false }
            else
              Below { skip_line = false }
          in
          (placement, mk_import ~bindings ~from)
      | (Default _, None, Some _) ->
        (* a `import {bar} from 'foo'` or `import * as Foo from 'foo'` already exists.
           rather than change it to `import Foo, {bar} from 'foo'`, we choose to insert
           a separate import. TODO: maybe make this a config option? *)
        (Above { skip_line = false }, mk_import ~bindings ~from)
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
        (Replace, stmt)
      | (Named _, Some _, _)
      | (Named _, None, Some (ImportNamespaceSpecifier _))
      | (NamedType _, Some _, _)
      | (NamedType _, None, Some (ImportNamespaceSpecifier _)) ->
        (* trying to insert a named specifier, but a default or namespace import already
           exists. rather than change it to `import Foo, {bar} from 'foo'`, we choose to
           insert a separate import `import {bar} from 'foo'` below the namespace/default
           import. TODO: maybe make this a config option? *)
        (Below { skip_line = false }, mk_import ~bindings ~from)
      | (Namespace _, Some _, _) ->
        (* trying to insert a namespace specifier, but a default import already exists.
           rather than change it to `import * as Foo, Bar from 'foo'`, we choose to
           insert a separate import `import * as Foo from 'foo'` below the default.
           TODO: maybe make this a config option? *)
        (Below { skip_line = false }, mk_import ~bindings ~from)
      | (Namespace _, None, Some (ImportNamedSpecifiers _)) ->
        (* trying to insert a namespace specifier, but a named import already exists.
           rather than change it to `import * as Foo, {bar} from 'foo'`, we choose to
           insert a separate import `import * as Foo from 'foo'` above the named import.
           TODO: maybe make this a config option? *)
        (Above { skip_line = false }, mk_import ~bindings ~from)
      | (_, None, None) -> failwith "unexpected import with neither a default nor specifiers"
    in
    (loc, edit)
  | _ -> failwith "trying to update a non-import"

(** default < namespace < named *)
let compare_kind_of_import a b =
  let open Statement in
  let open Statement.ImportDeclaration in
  let compare_identifier (_, { Identifier.name = a; _ }) (_, { Identifier.name = b; _ }) =
    String.compare a b
  in
  match (a, b) with
  | ( ( _,
        ImportDeclaration
          {
            import_kind = _;
            source = _;
            default = a_default;
            specifiers = a_specifiers;
            comments = _;
          } ),
      ( _,
        ImportDeclaration
          {
            import_kind = _;
            source = _;
            default = b_default;
            specifiers = b_specifiers;
            comments = _;
          } ) ) ->
    (match (a_default, b_default) with
    | (Some a_id, Some b_id) -> compare_identifier a_id b_id
    | (Some _, None) -> -1
    | (None, Some _) -> 1
    | (None, None) ->
      (match (a_specifiers, b_specifiers) with
      | (Some (ImportNamespaceSpecifier (_, a_id)), Some (ImportNamespaceSpecifier (_, b_id))) ->
        compare_identifier a_id b_id
      | (Some (ImportNamespaceSpecifier _), Some (ImportNamedSpecifiers _)) -> -1
      | (Some (ImportNamespaceSpecifier _), None) -> -1
      | (Some (ImportNamedSpecifiers _), Some (ImportNamedSpecifiers _)) -> 0
      | (Some (ImportNamedSpecifiers _), Some (ImportNamespaceSpecifier _)) -> 1
      | (Some (ImportNamedSpecifiers _), None) -> -1
      | (None, None) -> 0
      | (None, Some _) -> -1))
  | _ -> 0

let compare_imports a b =
  let k = Section.(compare (of_statement a) (of_statement b)) in
  if k = 0 then
    let k = ImportSource.(compare (of_statement a) (of_statement b)) in
    if k = 0 then
      compare_kind_of_import a b
    else
      k
  else
    k

(** [sorted_insertion_point import imports] walks [imports], ensuring it is
  sorted, and looking for the correct place to insert [import]. returns [None]
  if [imports] is not sorted, or [Some (loc, placement)] where [placement] is
  whether to insert above or below. *)
let sorted_insertion_point =
  let relative_placement import current =
    let k = Section.(compare (of_statement import) (of_statement current)) in
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
            Some (fst current, relative_placement import current)
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

let section_matches_bindings bindings section =
  let open Section in
  (* TODO: confirm CJS/ESM interop, depends on flowconfig *)
  match (section, bindings) with
  | (ImportType, NamedType _) -> true
  | (ImportType, (Default _ | Named _ | Namespace _)) -> false
  | (Require, Default _) -> true
  | (Require, (Named _ | NamedType _ | Namespace _)) -> false
  | ((ImportValueFromRelative | ImportValueFromModule), NamedType _) -> false
  | ((ImportValueFromRelative | ImportValueFromModule), (Default _ | Named _ | Namespace _)) -> true

let existing_import ~bindings ~from imports =
  let open Statement in
  let potentials =
    Base.List.filter
      ~f:(fun stmt ->
        section_matches_bindings bindings (Section.of_statement stmt)
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

let get_change ~imports ~body (from, bindings) =
  let (loc, placement, stmt) =
    match existing_import ~bindings ~from imports with
    | Some stmt ->
      let (loc, (placement, stmt)) = update_import ~bindings stmt in
      (loc, placement, stmt)
    | None ->
      let new_import = mk_import ~bindings ~from in
      let (loc, placement) = insertion_point ~imports ~body new_import in
      (loc, placement, new_import)
  in
  let loc =
    match placement with
    | Above _ -> Loc.start_loc loc
    | Below _ -> Loc.end_loc loc
    | Replace -> loc
  in
  (loc, (placement, stmt))

let string_of_change ~options (loc, (placement, stmt)) =
  let str = string_of_statement ~options stmt in
  let edit =
    match placement with
    | Above { skip_line } ->
      (* str doesn't include a newline, so we add one. if we want to skip
         a line, we add 2. *)
      if skip_line then
        str ^ "\n\n"
      else
        str ^ "\n"
    | Below { skip_line } ->
      (* loc of the previous statement doesn't include the trailing \n,
         so we add one to the beginning to replace it, and the existing
         one ends up trailing our inserted statement. if we want to skip
         a line, we add 2. *)
      if skip_line then
        "\n\n" ^ str
      else
        "\n" ^ str
    | Replace -> str
  in
  (loc, edit)

(** Ordering of placements, assuming they refer to the same Loc:
      1. Above { skip_line = true }
      2. Above { skip_line = false }
      3. Replace
      4. Below { skip_line = false }
      5. Below { skip_line = true }
  *)
let compare_placement p1 p2 =
  match (p1, p2) with
  | (Above { skip_line = s1 }, Above { skip_line = s2 }) -> Base.Bool.compare s2 s1
  | (Above _, (Below _ | Replace)) -> -1
  | (Below { skip_line = s1 }, Below { skip_line = s2 }) -> Base.Bool.compare s1 s2
  | (Below _, (Above _ | Replace)) -> 1
  | (Replace, Replace) -> 0
  | (Replace, Above _) -> 1
  | (Replace, Below _) -> -1

(** Orders changes based on where they'll appear in the updated file.

    For example, [(Above { skip_line = true }, import_bar)] is sorted before
    [(Above { skip_line = true }, import_foo)]. Both are sorted before
    [(Above { skip_line = false }, import_baz)] because they will end up
    with a blank line between them and [import_baz]. *)
let compare_changes (loc1, (placement1, stmt1)) (loc2, (placement2, stmt2)) =
  let k = Loc.compare loc1 loc2 in
  if k = 0 then
    let k = compare_placement placement1 placement2 in
    if k = 0 then
      compare_imports stmt1 stmt2
    else
      k
  else
    k

(** Given a list of changes, eliminates whitespace between imports in the same group.

    For example, consider these imports, inserted into a file with no existing imports:

      [import type {foo} from './foo']
      [import {bar} from './bar']
      [import {baz} from './baz']

    All 3 have [Above { skip_line = true }] because they're inserted at the top
    of the file, with a blank line between the first real code. But we don't want to
    render a line between [bar] and [baz] because they're both named imports, so we
    need to change [skip_line] to [false] for [bar]. But [foo] is a different group,
    so it should still skip a line; [baz] is the last one so it should still skip a
    line to separate the whole group from what follows. *)
let adjust_placements =
  let rec loop prev acc = function
    | [] -> Base.List.rev (prev :: acc)
    | next :: rest ->
      let (prev_loc, (prev_placement, prev_stmt)) = prev in
      let (next_loc, (next_placement, next_stmt)) = next in
      let (prev, next) =
        if
          Loc.equal prev_loc next_loc
          && compare_placement prev_placement next_placement = 0
          && Section.(compare (of_statement prev_stmt) (of_statement next_stmt)) = 0
        then
          (* being inserted into the same section, so eliminate the line between them *)
          let (prev_placement, next_placement) =
            match prev_placement with
            | Above _ -> (Above { skip_line = false }, next_placement)
            | Below _ -> (prev_placement, Below { skip_line = false })
            | Replace -> (prev_placement, next_placement)
          in
          let prev = (prev_loc, (prev_placement, prev_stmt)) in
          let next = (next_loc, (next_placement, next_stmt)) in
          (prev, next)
        else
          (prev, next)
      in
      loop next (prev :: acc) rest
  in
  function
  | [] -> []
  | hd :: tl -> loop hd [] tl

let add_imports ~options ~added_imports ast : (Loc.t * string) list =
  let (Flow_ast_differ.Partitioned { directives = _; imports; body }) =
    let (_, { Program.statements; _ }) = ast in
    Flow_ast_differ.partition_imports statements
  in
  added_imports
  |> Base.List.map ~f:(get_change ~imports ~body)
  |> Base.List.sort ~compare:compare_changes
  |> adjust_placements
  |> Base.List.map ~f:(string_of_change ~options)

let add_import ~options ~bindings ~from ast : (Loc.t * string) list =
  add_imports ~options ~added_imports:[(from, bindings)] ast

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
