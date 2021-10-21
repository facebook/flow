(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ast_info = (Loc.t, Loc.t) Flow_ast.Program.t * File_sig.With_Loc.t * Docblock.t

let compute_docblock file content =
  let open Parsing_service_js in
  let max_tokens = docblock_max_tokens in
  let (_errors, docblock) = parse_docblock ~max_tokens file content in
  docblock

(* We use compute_ast_result (as opposed to get_ast_result) when the file contents we have might be
 * different from what's on disk (and what is therefore stored in shared memory). This can be the
 * case for local find-refs requests, where the client may pipe in file contents rather than just
 * specifying a filename. For global find-refs, we assume that all dependent files are the same as
 * what's on disk, so we can grab the AST from the heap instead. *)
let compute_ast_result options file content =
  let docblock = compute_docblock file content in
  let open Parsing_service_js in
  let types_mode = TypesAllowed in
  let use_strict = true in
  let parse_options = make_parse_options ~fail:false ~types_mode ~use_strict docblock options in
  let result = do_parse ~parse_options ~info:docblock content file in
  match result with
  | Parse_ok { ast; file_sig; _ } -> Ok (ast, file_sig, docblock)
  (* The parse should not fail; we have passed ~fail:false *)
  | Parse_fail _ -> Error "Parse unexpectedly failed"
  | Parse_skip _ -> Error "Parse unexpectedly skipped"
