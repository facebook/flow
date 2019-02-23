(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  major: int;
  minor: int;
  patch: int;
}

let zero = {
  major = 0;
  minor = 0;
  patch = 0;
}

let compare
  { major = a_major; minor = a_minor; patch = a_patch }
  { major = b_major; minor = b_minor; patch = b_patch }
: int =
  Pervasives.compare (a_major, a_minor, a_patch) (b_major, b_minor, b_patch)

let incr_major { major; _ } =
  { zero with major = succ major }

let incr_minor { major; minor; _ } =
  { zero with major; minor = succ minor }

let incr_patch { major; minor; patch; _ } =
  { major; minor; patch = succ patch }

let to_string { major; minor; patch } =
  Printf.sprintf "%d.%d.%d" major minor patch
