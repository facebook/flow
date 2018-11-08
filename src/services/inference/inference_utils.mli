(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val error_of_docblock_error: source_file: File_key.t -> Parsing_service_js.docblock_error -> Errors.error
val set_of_docblock_errors: source_file: File_key.t -> Parsing_service_js.docblock_error list -> Errors.ErrorSet.t
val error_of_parse_error : source_file: File_key.t -> Loc.t * Parse_error.t -> Errors.error
val set_of_parse_error: source_file: File_key.t -> Loc.t * Parse_error.t -> Errors.ErrorSet.t
val error_of_file_sig_error : source_file: File_key.t -> File_sig.With_Loc.error -> Errors.error
val set_of_file_sig_error: source_file: File_key.t -> File_sig.With_Loc.error -> Errors.ErrorSet.t
val set_of_file_sig_tolerable_errors: source_file: File_key.t -> File_sig.With_ALoc.tolerable_error list -> Errors.ErrorSet.t
