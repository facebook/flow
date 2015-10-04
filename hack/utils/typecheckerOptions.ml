(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t = {
  (* When we encounter an unknown class|function|constant name outside
   * of strict mode, is that an error? *)
  tco_assume_php : bool;
  (* For somewhat silly historical reasons having to do with the lack
   * of .hhi's for fairly core XHP classes, we unfortunately mark all
   * XHP classes as not having their members fully known *)
  tco_unsafe_xhp : bool;

  (* List of <<UserAttribute>> names expected in the codebase *)
  tco_user_attrs : Utils.SSet.t option;
}

let empty = {
  tco_assume_php = true;
  tco_unsafe_xhp = false;
  tco_user_attrs = None;
}

let assume_php t = t.tco_assume_php
let unsafe_xhp t = t.tco_unsafe_xhp
let user_attrs t = t.tco_user_attrs
let allowed_attribute t name = match t.tco_user_attrs with
  | None -> true
  | Some attr_names -> Utils.SSet.mem name attr_names
