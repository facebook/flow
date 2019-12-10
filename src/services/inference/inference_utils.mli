(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

val error_of_package_json_error : source_file:File_key.t -> Loc.t * string -> ALoc.t Flow_error.t

val set_of_package_json_error : source_file:File_key.t -> Loc.t * string -> Flow_error.ErrorSet.t

val error_of_file_sig_error :
  source_file:File_key.t -> File_sig.With_Loc.error -> ALoc.t Flow_error.t

val set_of_file_sig_error :
  source_file:File_key.t -> File_sig.With_Loc.error -> Flow_error.ErrorSet.t

val set_of_file_sig_tolerable_errors :
  source_file:File_key.t -> File_sig.With_ALoc.tolerable_error list -> Flow_error.ErrorSet.t

val fold_whitelisted_well_formed_exports :
  f:(File_key.t -> 'a -> 'b -> 'b) -> Options.t -> 'a Utils_js.FilenameMap.t -> 'b -> 'b

val well_formed_exports_enabled : Options.t -> File_key.t -> bool
