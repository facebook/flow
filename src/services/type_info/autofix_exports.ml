(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module LocSet = Loc_collections.LocSet

let set_of_fixable_signature_verification_locations file_sig =
  File_sig.With_Loc.(
    Signature_builder_deps.Error.(
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

let fix_signature_verification_error_at_loc ~full_cx ~file_sig ~typed_ast =
  Insert_type.(
    insert_type
      ~full_cx
      ~file_sig
      ~typed_ast
      ~expand_aliases:false
      ~omit_targ_defaults:false
      ~strict:false
      ~ambiguity_strategy:Autofix_options.Generalize)

let fix_signature_verification_errors ~full_cx ~file_sig ~typed_ast =
  Insert_type.(
    let do_it = fix_signature_verification_error_at_loc ~full_cx ~file_sig ~typed_ast in
    let try_it loc (ast, it_errs) =
      try (do_it ast loc, it_errs)
      with FailedToInsertType err -> (ast, error_to_string err :: it_errs)
    in
    (fun ast locs -> LocSet.fold try_it locs (ast, [])))
