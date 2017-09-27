(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type request =
  | Subscribe
  | Autocomplete of (File_input.t * (* request id *) int)
  | DidOpen of (* filename *) string Nel.t
  | DidClose of (* filename *) string Nel.t

type response =
  | Errors of {errors: Errors.ErrorSet.t; warnings: Errors.ErrorSet.t}
  | StartRecheck
  | EndRecheck
  | AutocompleteResult of (ServerProt.autocomplete_response * (* request id *) int)
  | DidOpenAck
  | DidCloseAck
