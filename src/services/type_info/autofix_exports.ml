(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module LocSet = Loc_collections.LocSet

let set_of_fixable_signature_verification_locations file_sig =
  File_sig.With_Loc.(
    Signature_error.(
      let tolerable_errors = file_sig.File_sig.With_Loc.tolerable_errors in
      let add_fixable_sig_ver_error acc = function
        | SignatureVerificationError
            ( ExpectedAnnotation (loc, _)
            | UnexpectedExpression (loc, _)
            | UnexpectedObjectKey (loc, _)
            | UnexpectedObjectSpread (loc, _)
            | EmptyArray loc
            | EmptyObject loc
            | UnexpectedArraySpread (loc, _) ) ->
          LocSet.add loc acc
        | _ -> acc
      in
      List.fold_left add_fixable_sig_ver_error LocSet.empty tolerable_errors))

let fix_signature_verification_error_at_loc ?remote_converter ~full_cx ~file_sig ~typed_ast =
  let open Insert_type in
  insert_type
    ~full_cx
    ~file_sig
    ~typed_ast
    ~expand_aliases:false
    ?remote_converter
    ~omit_targ_defaults:false
    ~strict:false
    ~ambiguity_strategy:Autofix_options.Generalize

let fix_signature_verification_errors ~file_key ~full_cx ~file_sig ~typed_ast =
  let open Insert_type in
  let remote_converter =
    new ImportsHelper.remote_converter ~iteration:0 ~file:file_key ~reserved_names:SSet.empty
  in
  let try_it loc (ast, it_errs) =
    try
      ( fix_signature_verification_error_at_loc
          ~remote_converter
          ~full_cx
          ~file_sig
          ~typed_ast
          ast
          loc,
        it_errs )
    with FailedToInsertType err -> (ast, error_to_string err :: it_errs)
  in
  fun ast locs ->
    let ((loc, p), it_errors) = LocSet.fold try_it locs (ast, []) in
    let statements = add_imports remote_converter p.Flow_ast.Program.statements in
    let ast' = (loc, { p with Flow_ast.Program.statements }) in
    (ast', it_errors)
