(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Set.Make (IntKey)

let pp fmt iset =
  Format.fprintf fmt "@[<2>{";
  let elements = elements iset in
  (match elements with
  | [] -> ()
  | _ -> Format.fprintf fmt " ");
  ignore
    (List.fold_left
       (fun sep s ->
         if sep then Format.fprintf fmt ";@ ";
         Format.pp_print_int fmt s;
         true)
       false
       elements);
  (match elements with
  | [] -> ()
  | _ -> Format.fprintf fmt " ");
  Format.fprintf fmt "@,}@]"

let show iset = Format.asprintf "%a" pp iset

let to_string = show
