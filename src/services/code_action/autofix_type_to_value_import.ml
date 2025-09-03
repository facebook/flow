(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type action_on_specifiers =
  | DoNothing
  | RemoveEmptyNamedSpecifiers of {
      local: (Loc.t, Loc.t) Ast.Identifier.t option;
      remote: (Loc.t, Loc.t) Ast.Identifier.t;
    }
  | DropOneNamedSpecifier of {
      rest_specifiers: (Loc.t, Loc.t) Ast.Statement.ImportDeclaration.named_specifier list;
      local: (Loc.t, Loc.t) Ast.Identifier.t option;
      remote: (Loc.t, Loc.t) Ast.Identifier.t;
    }

class mapper ~target_loc =
  object (_this)
    inherit Flow_ast_contains_mapper.mapper target_loc

    method! statement_fork_point stmt =
      match stmt with
      | (loc, Ast.Statement.ImportDeclaration decl) ->
        let open Ast.Statement.ImportDeclaration in
        let { import_kind; source; specifiers; default; comments = _ } = decl in
        if not @@ Loc.contains loc target_loc then
          [stmt]
        else if import_kind = ImportType then
          let action =
            match specifiers with
            | None -> DoNothing
            | Some specifiers ->
              (match specifiers with
              | ImportNamespaceSpecifier _ -> DoNothing
              | ImportNamedSpecifiers named_specifiers ->
                let relevant_local_remote_pair = ref None in
                let named_specifiers' =
                  Base.List.filter
                    named_specifiers
                    ~f:(fun { kind; local; remote; remote_name_def_loc = _ } ->
                      match kind with
                      | Some ImportValue
                      | Some ImportTypeof ->
                        true
                      | None
                      | Some ImportType ->
                        let (id_loc, _) = Base.Option.value local ~default:remote in
                        if Loc.equal id_loc target_loc then (
                          relevant_local_remote_pair := Some (local, remote);
                          false
                        ) else
                          true
                  )
                in
                (match !relevant_local_remote_pair with
                | None -> DoNothing
                | Some (local, remote) ->
                  if Base.List.is_empty named_specifiers' then
                    RemoveEmptyNamedSpecifiers { local; remote }
                  else
                    DropOneNamedSpecifier { rest_specifiers = named_specifiers'; local; remote }))
          in
          match action with
          | RemoveEmptyNamedSpecifiers { local; remote } ->
            let new_stmt =
              ( loc,
                Ast.Statement.ImportDeclaration
                  {
                    import_kind = ImportValue;
                    default = None;
                    source;
                    specifiers =
                      Some
                        (ImportNamedSpecifiers
                           [{ kind = None; local; remote; remote_name_def_loc = None }]
                        );
                    comments = None;
                  }
              )
            in
            if Base.Option.is_none default then
              [new_stmt]
            else
              [(loc, Ast.Statement.ImportDeclaration { decl with specifiers = None }); new_stmt]
          | DropOneNamedSpecifier { rest_specifiers; local; remote } ->
            [
              ( loc,
                Ast.Statement.ImportDeclaration
                  { decl with specifiers = Some (ImportNamedSpecifiers rest_specifiers) }
              );
              ( loc,
                Ast.Statement.ImportDeclaration
                  {
                    import_kind = ImportValue;
                    default = None;
                    source;
                    specifiers =
                      Some
                        (ImportNamedSpecifiers
                           [{ kind = None; local; remote; remote_name_def_loc = None }]
                        );
                    comments = None;
                  }
              );
            ]
          | DoNothing ->
            (match default with
            | Some { identifier; remote_default_name_def_loc = _ } ->
              if Loc.equal (fst identifier) target_loc then
                if Base.Option.is_none specifiers then
                  [(loc, Ast.Statement.ImportDeclaration { decl with import_kind = ImportValue })]
                else
                  [
                    (loc, Ast.Statement.ImportDeclaration { decl with default = None });
                    ( loc,
                      Ast.Statement.ImportDeclaration
                        { decl with import_kind = ImportValue; specifiers = None }
                    );
                  ]
              else
                [stmt]
            | None -> [stmt])
        else if import_kind = ImportValue then
          let specifiers' =
            match specifiers with
            | None -> None
            | Some specifiers ->
              (match specifiers with
              | ImportNamespaceSpecifier _ -> Some specifiers
              | ImportNamedSpecifiers named_specifiers ->
                Some
                  (ImportNamedSpecifiers
                     (Base.List.map named_specifiers ~f:(fun named_specifier ->
                          let { kind; local; remote; remote_name_def_loc = _ } = named_specifier in
                          match kind with
                          | Some ImportValue
                          | Some ImportTypeof ->
                            named_specifier
                          | Some ImportType
                          | None ->
                            let (id_loc, _) = Base.Option.value local ~default:remote in
                            if Loc.equal id_loc target_loc then
                              { named_specifier with kind = None }
                            else
                              named_specifier
                      )
                     )
                  ))
          in
          if specifiers' <> specifiers then
            [(loc, Ast.Statement.ImportDeclaration { decl with specifiers = specifiers' })]
          else
            [stmt]
        else
          [stmt]
      | _ -> [stmt]
  end

let convert_type_to_value_import ast loc =
  let scope_info = Scope_builder.program ~enable_enums:true ~with_types:true ast in
  if Loc_collections.LocSet.mem loc (Scope_api.With_Loc.all_uses scope_info) then
    match Scope_api.With_Loc.def_of_use scope_info loc with
    | {
     Scope_api.With_Loc.Def.name = _;
     actual_name = _;
     kind = Bindings.Type { imported = true; type_only_namespace = false };
     locs = (def_loc, []);
    } ->
      let mapper = new mapper ~target_loc:def_loc in
      let ast' = mapper#program ast in
      if ast' == ast then
        None
      else
        Some ast'
    | _ -> None
  else
    None
