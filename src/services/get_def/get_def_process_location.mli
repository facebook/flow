(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type internal_error =
  | Enclosing_node_error
  | On_demand_tast_error
[@@deriving show]

type 'loc result =
  | OwnNamedDef of 'loc * (* name *) string
  | OwnUnnamedDef of 'loc
  | ModuleDef of Type.t
  | Request of ('loc, 'loc * (Type.t[@opaque])) Get_def_request.t
  | Empty of string
  | LocNotFound
  | InternalError of internal_error
[@@deriving show]

(* We don't really need to export this type, but it is a convenient way to enforce
 * that searcher is indeed polymorpphic over 'T, instead of being pinned to
 * `ALoc.t * Type.t` through an inferred constraint. *)
class virtual ['T] searcher :
  'c
  -> is_local_use:(ALoc.t -> bool)
  -> is_legit_require:(ALoc.t -> bool)
  -> covers_target:(ALoc.t -> bool)
  -> purpose:Get_def_types.Purpose.t
  -> object
       inherit [ALoc.t, 'T, ALoc.t, 'T] Typed_ast_finder.enclosing_node_mapper

       val mutable available_private_names : ALoc.t SMap.t

       val mutable enclosing_node_stack : (ALoc.t, 'T) Typed_ast_finder.enclosing_node list

       val mutable found_loc_ : ALoc.t result

       val mutable in_require_declarator : bool

       method virtual private type_from_enclosing_node : 'T -> Type.t

       method virtual private get_module_t : 'T -> ALoc.t Ast.StringLiteral.t -> Type.t

       method virtual private remote_name_def_loc_of_import_named_specifier :
         (ALoc.t, 'T) Ast.Statement.ImportDeclaration.named_specifier -> ALoc.t option

       method virtual private imported_name_def_loc_of_export_named_declaration_specifier :
         (ALoc.t, 'T) Ast.Statement.ExportNamedDeclaration.ExportSpecifier.t -> ALoc.t option

       method virtual private remote_default_name_def_loc_of_import_declaration :
         ALoc.t * (ALoc.t, 'T) Ast.Statement.ImportDeclaration.t -> ALoc.t option

       method virtual private component_name_of_jsx_element :
         'T -> (ALoc.t, 'T) Ast.JSX.element -> ALoc.t * Type.t

       method found_loc : ALoc.t result

       method virtual loc_of_annot : 'T -> ALoc.t
     end

val process_type_request : Context.t -> Type.t -> (ALoc.t, string) Stdlib.result

val process_location :
  Context.t ->
  available_ast:Typed_ast_utils.available_ast ->
  is_local_use:(ALoc.t -> bool) ->
  is_legit_require:(ALoc.t -> bool) ->
  purpose:Get_def_types.Purpose.t ->
  Loc.t ->
  ALoc.t result
