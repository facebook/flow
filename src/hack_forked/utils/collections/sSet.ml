(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Flow_set.Make (StringKey)

let pp fmt sset =
  Format.fprintf fmt "@[<2>{";
  let elements = elements sset in
  (match elements with
  | [] -> ()
  | _ -> Format.fprintf fmt " ");
  ignore
    (List.fold_left
       (fun sep s ->
         if sep then Format.fprintf fmt ";@ ";
         Format.fprintf fmt "%S" s;
         true)
       false
       elements);
  (match elements with
  | [] -> ()
  | _ -> Format.fprintf fmt " ");
  Format.fprintf fmt "@,}@]"

let show sset = Format.asprintf "%a" pp sset

let to_string = show
