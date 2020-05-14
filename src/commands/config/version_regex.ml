(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* given a string like "1.2.3", generates a regex that matches all versions
   less than or equal to the given version. for example, it should match 0, 1,
   0.\d+, 1.0, 1.1, 1.2, 0.\d+.\d+, 1.0.\d+, 1.1.\d+, or 1.2.[0-3].

   a complete example for "23.45.6":

    23 (\.                                          # 23
      (
        45 (\.[0-6])? |                             # 23.45, or 23.45.0 - 23.45.6
        (4[0-4] | [0-3][0-9] | [0-9]) (\.[0-9]+)?   # 23.0 - 23.44, or 23.0.\d+ - 23.44.\d+
      )
    )?
    |
    (2[0-2] | [0-1][0-9] | [0-9])                   # 0-22
      (\.[0-9]+)? (\.[0-9]+)?                       # 0.\d+ - 22\.d+, or 0.\d+.\d+ - 22.\d+.\d+

   *)
let less_than_or_equal_to_version =
  let rec all_nines str i len =
    if i >= len then
      true
    else if str.[i] = '9' then
      (all_nines [@tailcall]) str (succ i) len
    else
      false
  in
  let range_of_digit digit =
    if digit = '0' then
      "0"
    else
      "[0-" ^ String.make 1 digit ^ "]"
  in
  let union parts =
    if List.length parts = 1 then
      List.hd parts
    else
      "\\(" ^ String.concat "\\|" parts ^ "\\)"
  in
  let n_digits n = List.init n (fun _ -> "[0-9]") |> String.concat "" in
  let n_or_fewer_digits n = "[0-9]" ^ (List.init (n - 1) (fun _ -> "[0-9]?") |> String.concat "") in
  let rec range ~pad part =
    let len = String.length part in
    if len = 1 then
      [range_of_digit part.[0]]
    else if all_nines part 0 len then
      [n_or_fewer_digits len]
    else
      let msd_str = String.sub part 0 1 in
      let rest = String.sub part 1 (len - 1) in
      let msd = Base.Int.of_string msd_str in
      (* for "234", match 200-234 *)
      let x = msd_str ^ union (range ~pad:true rest) in
      if msd > 1 then
        (* for 234, match [0-1][0-9][0-9] *)
        let prev_msd_range = range_of_digit (Char.chr (Char.code '0' + pred msd)) in
        let prev_range = prev_msd_range ^ n_digits (len - 1) in
        (* for 234, match [0-9][0-9] and [0-9] *)
        let rest_range = n_or_fewer_digits (len - 1) in
        [x; prev_range; rest_range]
      else if msd = 1 then
        let rest_range = range ~pad:false (String.make (len - 1) '9') in
        if pad then
          let prev_range = "0" ^ n_digits (len - 1) in
          x :: prev_range :: rest_range
        else
          x :: rest_range
      else
        [x]
  in
  let rec helper : int list -> string = function
    | [] -> ""
    | [part] -> union (range ~pad:false (string_of_int part))
    | part :: rest ->
      let str = string_of_int part in
      let x = str ^ "\\(\\." ^ helper rest ^ "\\)?" in
      if part = 0 then
        x
      else
        let prev = pred part |> string_of_int in
        let rest = Base.List.map ~f:(fun _ -> "\\(\\.[0-9]+\\)?") rest in
        union [x; union (range ~pad:false prev) ^ String.concat "" rest]
  in
  fun version ->
    let parts =
      try Scanf.sscanf version "%u.%u.%u" (fun major minor patch -> [major; minor; patch]) with
      | End_of_file ->
        raise (Failure ("Unable to parse version " ^ version ^ ": does not match \"%u.%u.%u\""))
      | Scanf.Scan_failure err
      | Failure err ->
        raise (Failure ("Unable to parse version " ^ version ^ ": " ^ err))
    in
    helper parts
