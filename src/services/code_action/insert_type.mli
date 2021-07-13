(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
  | MulipleTypesPossibleAtPoint of {
      generalized: (Loc.t, Loc.t) Flow_ast.Type.t;
      specialized: (Loc.t, Loc.t) Flow_ast.Type.t;
    }
  | FailedToValidateType of {
      error: Insert_type_utils.Error.validation_error;
      error_message: string;
    }
  | FailedToTypeCheck of Errors.ConcreteLocPrintableErrorSet.t
  | FailedToNormalize of (Loc.t * string)
  | FailedToImport of Insert_type_utils.Error.import_error

type errors =
  | Unexpected of unexpected
  | Expected of expected

exception FailedToInsertType of errors

exception FoundAmbiguousType

val simplify : Ty.t -> Ty.t

val path_of_loc : ?error:(string, string) result -> Loc.t -> (string, string) result

val error_to_string : errors -> string

val synth_type :
  ?size_limit:int ->
  full_cx:Context.t ->
  file_sig:File_sig.With_ALoc.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  expand_aliases:bool ->
  omit_targ_defaults:bool ->
  ambiguity_strategy:Autofix_options.ambiguity_strategy ->
  remote_converter:Insert_type_imports.ImportsHelper.remote_converter ->
  Loc.t ->
  Type.TypeScheme.t ->
  (Loc.t, Loc.t) Flow_ast.Type.annotation

val add_statement_after_directive_and_type_imports :
  (Loc.t, Loc.t) Flow_ast.Statement.t list ->
  (Loc.t, Loc.t) Flow_ast.Statement.t list ->
  (Loc.t, Loc.t) Flow_ast.Statement.t list

val add_imports :
  Insert_type_imports.ImportsHelper.remote_converter ->
  (Loc.t, Loc.t) Flow_ast.Statement.t list ->
  (Loc.t, Loc.t) Flow_ast.Statement.t list

val insert_type :
  full_cx:Context.t ->
  file_sig:File_sig.With_Loc.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  expand_aliases:bool ->
  omit_targ_defaults:bool ->
  strict:bool ->
  ambiguity_strategy:Autofix_options.ambiguity_strategy ->
  ?remote_converter:Insert_type_imports.ImportsHelper.remote_converter ->
  (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t ->
  Loc.t ->
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
