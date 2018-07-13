(**
 * Copyright (c) 2013-present, Facebook, Inc.
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
  let int_of_string x =
    try Pervasives.int_of_string x with Failure _ -> raise (Failure ("int_of_string: " ^ x))
  in
  let rec all_nines str i len =
    if i >= len then true
    else if str.[i] = '9' then (all_nines [@tailcall]) str (succ i) len
    else false
  in
  let range_of_digit digit = if digit = '0' then "0" else "[0-"^(String.make 1 digit)^"]" in
  let union parts =
    if List.length parts = 1 then List.hd parts
    else "\\(" ^ (String.concat "\\|" parts) ^ "\\)"
  in
  let rec range part =
    let len = String.length part in
    if len = 1 then
      [range_of_digit part.[0]]
    else if all_nines part 0 len then
      let rest = range (String.sub part 1 (len - 1)) in
      ("[0-9]" ^ (union rest))::rest
    else begin
      let msd_str = String.sub part 0 1 in
      let rest = String.sub part 1 (len - 1) in
      let msd = int_of_string msd_str in
      let x = msd_str ^ (union (range rest)) in
      if msd > 1 then
        let prev_msd_range = range_of_digit (Char.chr (Char.code '0' + (pred msd))) in
        let rest = String.make (len - 1) '9' in
        let rest_range = range rest in
        let prev_range = prev_msd_range ^ (union rest_range) in
        x::prev_range::rest_range
      else if msd = 1 then begin
        let rest = String.make (len - 1) '9' in
        x::(range rest)
      end else [x]
    end
  in
  let rec helper = function
  | [] -> ""
  | part::[] -> union (range part)
  | part::rest ->
      let x = part ^ "\\(\\." ^ (helper rest) ^ "\\)?" in
      if part = "0" then
        x
      else
        let prev = part |> int_of_string |> pred |> string_of_int in
        let rest = List.map (fun _ -> "\\(\\.[0-9]+\\)?") rest in
        union [x; (union (range prev)) ^ (String.concat "" rest)]
  in
  fun version ->
    version
    |> Str.split (Str.regexp_string ".")
    |> helper
