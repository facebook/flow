(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let soi = string_of_int
let string_of_char = String.make 1

let string_before s n = String.sub s 0 n
let string_after s n = String.sub s n (String.length s - n)

let string_starts_with long short =
  try
    let long = String.sub long 0 (String.length short) in
    long = short
  with Invalid_argument _ ->
    false

let string_ends_with long short =
  try
    let len = String.length short in
    let long = String.sub long (String.length long - len) len in
    long = short
  with Invalid_argument _ ->
    false

(* Returns the index of the first occurrence of string `needle` in string
   `haystack`. If not found, returns -1.

   An implementation of the Knuth-Morris-Pratt (KMP) algorithm. *)
let substring_index needle =
  (* see Wikipedia pseudocode *)
  let needle_len = String.length needle in
  if needle_len = 0 then raise (Invalid_argument needle);
  let table = Array.make needle_len 0 in
  table.(0) <- (-1);
  let pos = ref 2 and cnd = ref 0 in
  while !pos < needle_len do
    if needle.[!pos - 1] = needle.[!cnd] then
      (table.(!pos) <- !cnd + 1; incr pos; incr cnd)
    else if !cnd > 0 then
      cnd := table.(!cnd)
    else
      (table.(!pos) <- 0; incr pos)
  done;
  fun haystack ->
    let len = String.length haystack in
    let p = ref 0 in
    let q = ref 0 in
    while !p < len && !q < needle_len do
      if haystack.[!p] = needle.[!q] then (incr p; incr q)
      else if !q = 0 then incr p
      else q := table.(!q)
    done;
    if !q >= needle_len then !p - needle_len
    else -1

let is_substring needle =
  let substring_index_memo = substring_index needle in
  fun haystack -> (substring_index_memo haystack) >= 0

(* Return a copy of the string with prefixing string removed.
 * The function is a no-op if it s does not start with prefix.
 * Modeled after Python's string.lstrip.
 *)
let lstrip s prefix =
  let prefix_length = String.length prefix in
  if string_starts_with s prefix
  then String.sub s prefix_length (String.length s - prefix_length)
  else s

let rpartition s c =
  let sep_idx = String.rindex s c in
  let first = String.sub s 0 sep_idx in
  let second =
    String.sub s (sep_idx + 1) (String.length s - sep_idx - 1) in
  first, second

let is_lowercase_char =
  let a_code, z_code = Char.code 'a', Char.code 'z' in
  fun chr ->
    let code = Char.code chr in
    a_code <= code && code <= z_code

let rec is_not_lowercase str i j =
  if is_lowercase_char str.[i] then false
  else if i = j then true
  else is_not_lowercase str (i + 1) j

(* String provides map and iter but not fold. It also is missing a char_list_of
 * function. Oh well. You can use fold to simulate anything you need, I suppose
 *)
let fold_left ~f ~acc str =
  let acc = ref acc in
  String.iter (fun c -> acc := f (!acc) c) str;
  !acc

(* Replaces all instances of the needle character with the replacement character
 *)
let replace_char needle replacement =
  String.map (fun c -> if c = needle then replacement else c)

(* Splits a string into a list of strings using "\n", "\r" or "\r\n" as
 * delimeters. If the string starts or ends with a delimeter, there WILL be an
 * empty string at the beginning or end of the list, like Str.split_delim does
 *)
let split_into_lines str =
  (* Fold through the list and break the string into a reversed list of
   * reversed char lists *)
  let _, (partial, lines) = fold_left
    ~f: (fun (idx, (partial, lines)) c ->
      (* For \r\n, we've already processed the newline *)
      if c = '\n' && idx > 0 && String.get str (idx-1) = '\r'
      then idx+1, (partial, lines)
      else
        if c = '\n' || c = '\r'
        then idx+1, ([], partial::lines)
        else idx+1, (c::partial, lines))
    ~acc: (0, ([], []))
    str in

  (* Reverse everything and turn the char lists into strings *)
  List.fold_left (fun lines chars ->
    let line = chars
    |> List.rev
    |> List.map (String.make 1)
    |> String.concat "" in
    line::lines
  ) [] (partial::lines)
