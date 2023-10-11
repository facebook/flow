(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_errors_utils
open Utils_js

type t = {
  collated_duplicate_providers_errors: (Loc.t printable_error * File_key.t * File_key.t) list;
  collated_local_errors: ConcreteLocPrintableErrorSet.t Utils_js.FilenameMap.t;
  collated_merge_errors: ConcreteLocPrintableErrorSet.t Utils_js.FilenameMap.t;
  collated_warning_map: ConcreteLocPrintableErrorSet.t Utils_js.FilenameMap.t;
  collated_suppressed_errors:
    (Loc.t printable_error * Loc_collections.LocSet.t) list Utils_js.FilenameMap.t;
}

let empty =
  {
    collated_duplicate_providers_errors = [];
    collated_local_errors = FilenameMap.empty;
    collated_merge_errors = FilenameMap.empty;
    collated_warning_map = FilenameMap.empty;
    collated_suppressed_errors = FilenameMap.empty;
  }

let clear_all files errors =
  FilenameSet.fold
    (fun file
         {
           collated_duplicate_providers_errors;
           collated_local_errors;
           collated_merge_errors;
           collated_warning_map;
           collated_suppressed_errors;
         } ->
      let collated_duplicate_providers_errors =
        Base.List.filter
          ~f:(fun (_, f1, f2) -> f1 <> file && f2 <> file)
          collated_duplicate_providers_errors
      in
      {
        collated_duplicate_providers_errors;
        collated_local_errors = FilenameMap.remove file collated_local_errors;
        collated_merge_errors = FilenameMap.remove file collated_merge_errors;
        collated_warning_map = FilenameMap.remove file collated_warning_map;
        collated_suppressed_errors = FilenameMap.remove file collated_suppressed_errors;
      })
    files
    errors

let clear_merge files errors =
  FilenameSet.fold
    (fun file
         {
           collated_duplicate_providers_errors;
           collated_local_errors;
           collated_merge_errors;
           collated_warning_map;
           collated_suppressed_errors;
         } ->
      {
        collated_duplicate_providers_errors;
        collated_local_errors;
        collated_merge_errors = FilenameMap.remove file collated_merge_errors;
        collated_warning_map = FilenameMap.remove file collated_warning_map;
        collated_suppressed_errors = FilenameMap.remove file collated_suppressed_errors;
      })
    files
    errors
