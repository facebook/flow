(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module T = Ast.Type
module F = T.Function
module Id = Ast.Identifier
module TP = T.TypeParams
module GId = T.Generic.Identifier

module RemoveReactImportStats = struct
  type t = {
    removed: int;
    still_useful: int;
  }

  let empty = { removed = 0; still_useful = 0 }

  let combine x1 x2 =
    { removed = x1.removed + x2.removed; still_useful = x1.still_useful + x2.still_useful }

  let add_removed x = { x with removed = x.removed + 1 }

  let add_still_useful x = { x with still_useful = x.still_useful + 1 }

  let serialize x =
    [Utils_js.spf "removed: %d" x.removed; Utils_js.spf "still useful: %d" x.still_useful]

  let report x =
    [
      Insert_type_utils.string_of_row ~indent:2 "Removed instances" x.removed;
      Insert_type_utils.string_of_row ~indent:2 "Still useful instances" x.still_useful;
    ]
end

module Acc = Insert_type_utils.UntypedAcc (RemoveReactImportStats)

let react_import_def_loc_opt_of_stmt = function
  | ( _,
      Ast.Statement.ImportDeclaration
        {
          Ast.Statement.ImportDeclaration.import_kind = Ast.Statement.ImportDeclaration.ImportValue;
          source = (_, { Ast.StringLiteral.value = "react"; _ });
          default =
            Some
              {
                Ast.Statement.ImportDeclaration.identifier =
                  (loc, { Ast.Identifier.name = "React"; _ });
                _;
              };
          specifiers = None;
          comments = _;
        }
    )
  | ( _,
      Ast.Statement.ImportDeclaration
        {
          Ast.Statement.ImportDeclaration.import_kind = Ast.Statement.ImportDeclaration.ImportValue;
          source = (_, { Ast.StringLiteral.value = "react"; _ });
          default = None;
          specifiers =
            Some
              (Ast.Statement.ImportDeclaration.ImportNamespaceSpecifier
                (_, (loc, { Ast.Identifier.name = "React"; _ }))
                );
          comments = _;
        }
    ) ->
    Some loc
  | _ -> None

let has_unaccounted_react_value_usage_visitor =
  object (this)
    inherit [bool, Loc.t] Flow_ast_visitor.visitor ~init:false as super

    val mutable in_typeof_type = false

    method! typeof_type t =
      let saved_in_typeof_type = in_typeof_type in
      in_typeof_type <- true;
      let t' = super#typeof_type t in
      in_typeof_type <- saved_in_typeof_type;
      t'

    method! component_declaration loc c =
      this#update_acc (fun _ -> true);
      super#component_declaration loc c

    method! identifier ((_, { Ast.Identifier.name; _ }) as id) =
      if in_typeof_type && name = "React" then this#update_acc (fun _ -> true);
      id
  end

let mapper ctx =
  object (this)
    inherit [Acc.t] Codemod_ast_mapper.mapper "" ~init:Acc.empty

    method! program ((prog_loc, ({ Ast.Program.statements; _ } as prog')) as prog) =
      let file = ctx.Codemod_context.Untyped.file in
      let react_import_def_loc =
        Base.List.fold statements ~init:None ~f:(fun acc stmt ->
            if Base.Option.is_some acc then
              acc
            else
              react_import_def_loc_opt_of_stmt stmt
        )
      in
      match react_import_def_loc with
      | None -> prog
      | Some react_import_def_loc ->
        (* We intentionally exclude types so that the type uses of React is not counted. *)
        let scope_info = Scope_builder.program ~enable_enums:false ~with_types:false prog in
        let unused =
          Scope_api.With_Loc.def_is_unused
            scope_info
            (Scope_api.With_Loc.def_of_use scope_info react_import_def_loc)
          && not
               (has_unaccounted_react_value_usage_visitor#eval
                  has_unaccounted_react_value_usage_visitor#program
                  prog
               )
        in
        this#update_acc (fun acc ->
            let extra =
              if unused then
                RemoveReactImportStats.add_removed acc.Acc.stats
              else
                RemoveReactImportStats.add_still_useful acc.Acc.stats
            in
            Acc.update_stats acc extra
        );
        if unused then (
          this#update_acc (fun acc ->
              { acc with Acc.changed_set = Utils_js.FilenameSet.add file acc.Acc.changed_set }
          );
          ( prog_loc,
            {
              prog' with
              Ast.Program.statements =
                Base.List.filter statements ~f:(fun stmt ->
                    Base.Option.is_none (react_import_def_loc_opt_of_stmt stmt)
                );
            }
          )
        ) else
          prog
  end
