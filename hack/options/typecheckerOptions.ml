(**
 * Copyright (c) 2015, Facebook, Inc.
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
   * XHP classes as not having their members fully known at Facebook.
   *
   * We've also historically not typechecked attributes, and using string
   * literals for things that aren't strings (eg colspan="3") is common.
   *
   * If set to true, this option disables the new, stricter behavior. Please
   * don't use for new code, but it's useful for migrating existing XHP
   * codebases. *)
  tco_unsafe_xhp : bool;

  (* List of <<UserAttribute>> names expected in the codebase *)
  tco_user_attrs : SSet.t option;

  (* Set of experimental features, in lowercase. *)
  tco_experimental_features : SSet.t;

  (* Namespace map that allows supporting namespace aliasing *)
  tco_auto_namespace_map : (string * string) list;
}

(* Currently there is only one experimental feature *)
let experimental_dict = "dict"
let experimental_all = SSet.add experimental_dict SSet.empty

let default = {
  tco_assume_php = true;
  tco_unsafe_xhp = false;
  tco_user_attrs = None;
  (** Default all features for testing. Actual options are set by reading
   * from hhconfig, which defaults to empty. *)
  tco_experimental_features = experimental_all;
  tco_auto_namespace_map = [];
}

(* Use this instead of default when you don't have access to a project .hhconfig
 * and it's acceptable to omit some errors. IDE integration is one example,
 * assuming the IDE also talks to hh_client to get the full list with higher
 * latency.
 *
 * Use 'default' if it's fine to be potentially stricter than the rest of the
 * project requires, eg hh_single_typecheck *)
let permissive = { default with tco_unsafe_xhp = true; }

let assume_php t = t.tco_assume_php
let unsafe_xhp t = t.tco_unsafe_xhp
let user_attrs t = t.tco_user_attrs
let allowed_attribute t name = match t.tco_user_attrs with
  | None -> true
  | Some attr_names -> SSet.mem name attr_names
let experimental_feature_enabled t s = SSet.mem s t.tco_experimental_features
let auto_namespace_map t = t.tco_auto_namespace_map
