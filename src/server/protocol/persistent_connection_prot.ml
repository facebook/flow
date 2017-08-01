(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
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
