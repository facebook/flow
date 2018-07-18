(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

include Set.Make (StringKey)
let to_string sset =
  "{" ^ (String.concat "," (elements sset)) ^ "}"

let pp fmt sset =
  Format.fprintf fmt "@[<2>{";
  ignore
    (List.fold_left
      (fun sep s ->
        if sep then Format.fprintf fmt ";@ ";
        Format.fprintf fmt "%S" s;
        true)
      false
      (elements sset));
  Format.fprintf fmt "@,}@]"
