(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type type_contents_artifacts =
  | Type_contents_artifacts of {
      cx: Context.t;
      docblock: Docblock.t;
      docblock_errors: Parsing_service_js.docblock_error list;
      file_sig: File_sig.With_Loc.t;
      tolerable_errors: File_sig.With_Loc.tolerable_error list;
      ast: (Loc.t, Loc.t) Flow_ast.Program.t;
      typed_ast: (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t;
      parse_errors: (Loc.t * Parse_error.t) list;
    }
