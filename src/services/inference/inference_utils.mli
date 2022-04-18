(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val error_of_docblock_error :
  source_file:File_key.t -> Parsing_service_js.docblock_error -> ALoc.t Flow_error.t

val set_of_docblock_errors :
  source_file:File_key.t -> Parsing_service_js.docblock_error list -> Flow_error.ErrorSet.t

val error_of_parse_error : source_file:File_key.t -> Loc.t * Parse_error.t -> ALoc.t Flow_error.t

val set_of_parse_error : source_file:File_key.t -> Loc.t * Parse_error.t -> Flow_error.ErrorSet.t

val error_of_parse_exception : source_file:File_key.t -> Exception.t -> ALoc.t Flow_error.t

val set_of_parse_exception : source_file:File_key.t -> Exception.t -> Flow_error.ErrorSet.t

val set_of_file_sig_tolerable_errors :
  source_file:File_key.t -> File_sig.With_ALoc.tolerable_error list -> Flow_error.ErrorSet.t
