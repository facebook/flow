(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let string name used_names =
  let rec loop i =
    let candidate =
      if i = 0 then
        name ^ "_"
      else
        Utils_js.spf "%s_%d" name i
    in
    if SSet.mem candidate used_names then
      loop (i + 1)
    else
      candidate
  in
  loop 0

let subst_name name used_names =
  let (ct, n) =
    match name with
    | Subst_name.Synthetic { name = n; _ } ->
      failwith (Utils_js.spf "Cannot rename synthetic name %s" n)
    | Subst_name.Name n -> (0, n)
    | Subst_name.Id (ct, n) -> (ct, n)
  in
  let rec loop ct =
    let name = Subst_name.Id (ct, n) in
    if not @@ Subst_name.Set.mem name used_names then
      name
    else
      loop (ct + 1)
  in
  loop (ct + 1)

let avoid_capture ~name ~map ~in_free_vars ~used_names ~fresh_name ~remove ~add_alpha =
  if in_free_vars name then
    let new_name = fresh_name name (used_names ()) in
    (new_name, add_alpha name new_name map)
  else
    (name, remove name map)
