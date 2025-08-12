(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type unexpected =
  | UnknownTypeAtPoint of Loc.t
  | NoFileInLocation of Loc.t
  | FailedToSerialize of {
      ty: Ty.t;
      error_message: string;
    }
  | FailedToNormalizeNoMatch

type expected =
  | TypeAnnotationAtPoint of {
      location: Loc.t;
      type_ast: (Loc.t, Loc.t) Flow_ast.Type.t;
    }
  | InvalidAnnotationTarget of Loc.t
  | UnsupportedAnnotation of {
      location: Loc.t;
      error_message: string;
    }
  | MultipleTypesPossibleAtPoint of {
      generalized: (Loc.t, Loc.t) Flow_ast.Type.t;
      specialized: (Loc.t, Loc.t) Flow_ast.Type.t;
    }
  | FailedToValidateType of {
      error: Insert_type_utils.Error.validation_error;
      error_message: string;
    }
  | FailedToTypeCheck of Flow_errors_utils.ConcreteLocPrintableErrorSet.t
  | FailedToNormalize of (Loc.t * string)
  | FailedToImport of Insert_type_utils.Error.import_error

type errors =
  | Unexpected of unexpected
  | Expected of expected

exception FailedToInsertType of errors

val simplify : Ty.t -> Ty.t

val path_of_loc : ?error:(string, string) result -> Loc.t -> (string, string) result

val error_to_string : errors -> string

val synth_type :
  ?size_limit:int ->
  cx:Context.t ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  get_ast_from_shared_mem:(File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option) ->
  file_sig:File_sig.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  omit_targ_defaults:bool ->
  ambiguity_strategy:Autofix_options.ambiguity_strategy ->
  remote_converter:Insert_type_imports.ImportsHelper.remote_converter ->
  Loc.t ->
  Type.t ->
  ((Loc.t, Loc.t) Flow_ast.Type.annotation, expected) result

val add_statement_after_directive_and_type_imports :
  (Loc.t, Loc.t) Flow_ast.Statement.t list ->
  (Loc.t, Loc.t) Flow_ast.Statement.t list ->
  (Loc.t, Loc.t) Flow_ast.Statement.t list

val add_imports :
  Insert_type_imports.ImportsHelper.remote_converter ->
  (Loc.t, Loc.t) Flow_ast.Statement.t list ->
  (Loc.t, Loc.t) Flow_ast.Statement.t list

val insert_type :
  cx:Context.t ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  get_ast_from_shared_mem:(File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option) ->
  get_haste_module_info:(File_key.t -> Haste_module_info.t option) ->
  get_type_sig:(File_key.t -> Type_sig_collections.Locs.index Packed_type_sig.Module.t option) ->
  file_sig:File_sig.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  omit_targ_defaults:bool ->
  strict:bool ->
  ambiguity_strategy:Autofix_options.ambiguity_strategy ->
  ?remote_converter:Insert_type_imports.ImportsHelper.remote_converter ->
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t

val insert_type_t :
  cx:Context.t ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  get_ast_from_shared_mem:(File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option) ->
  get_haste_module_info:(File_key.t -> Haste_module_info.t option) ->
  get_type_sig:(File_key.t -> Type_sig_collections.Locs.index Packed_type_sig.Module.t option) ->
  file_sig:File_sig.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  omit_targ_defaults:bool ->
  strict:bool ->
  ambiguity_strategy:Autofix_options.ambiguity_strategy ->
  ?remote_converter:Insert_type_imports.ImportsHelper.remote_converter ->
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t ->
  Loc.t ->
  Type.t ->
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t

val mk_diff :
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  Flow_ast_differ.node Flow_ast_differ.changes

val mk_patch :
  opts:Js_layout_generator.opts ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  string ->
  Replacement_printer.patch
