(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Digest

let to_raw_contents x = x

(* take the raw contents of the Digest
   and convert it into a digest if it would be capable of being
   to_hex'd *)
let from_raw_contents x =
  try
    let (_ : string) = to_hex x in
    Some x
  with Invalid_argument _ -> None
