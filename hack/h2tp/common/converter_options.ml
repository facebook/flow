(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module CE = Common_exns

type options = {
  convert_collections: bool;
}

let options_ref = ref None

let set opts = match !options_ref with
  | None -> options_ref := Some opts
  | Some _ -> raise
      (CE.AssertionError "conversion options should only be set once")

let get () = match !options_ref with
  | None -> raise
      (CE.AssertionError "conversion options have not been set")
  | Some opts -> opts
