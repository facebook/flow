(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type parse_artifacts =
  | Parse_artifacts of {
      docblock: Docblock.t;
      docblock_errors: Parsing_service_js.docblock_error list;
      ast: (Loc.t, Loc.t) Flow_ast.Program.t;
      file_sig: File_sig.With_Loc.t;
      tolerable_errors: File_sig.With_Loc.tolerable_error list;
      parse_errors: (Loc.t * Parse_error.t) list;
    }

type typecheck_artifacts =
  | Typecheck_artifacts of {
      cx: Context.t;
      typed_ast: (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t;
    }

type type_contents_artifacts = parse_artifacts * typecheck_artifacts
