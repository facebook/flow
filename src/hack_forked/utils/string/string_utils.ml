(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception Incorrect_format

let soi = string_of_int

let string_of_char = String.make 1

let string_before s n = String.sub s 0 n

let string_after s n = String.sub s n (String.length s - n)

let string_starts_with long short =
  try
    let long = String.sub long 0 (String.length short) in
    long = short
  with Invalid_argument _ -> false

let string_ends_with long short =
  try
    let len = String.length short in
    let long = String.sub long (String.length long - len) len in
    long = short
  with Invalid_argument _ -> false

(** [substring_index needle haystack] returns the index of the first occurrence of
    string [needle] in string [haystack]. If not found, returns [-1].

    An implementation of the Knuth-Morris-Pratt (KMP) algorithm. *)
let substring_index needle =
  (* see Wikipedia pseudocode *)
  let needle_len = String.length needle in
  if needle_len = 0 then raise (Invalid_argument needle);
  let table = Array.make needle_len 0 in
  table.(0) <- -1;
  let pos = ref 2 and cnd = ref 0 in
  while !pos < needle_len do
    if needle.[!pos - 1] = needle.[!cnd] then (
      table.(!pos) <- !cnd + 1;
      incr pos;
      incr cnd
    ) else if !cnd > 0 then
      cnd := table.(!cnd)
    else (
      table.(!pos) <- 0;
      incr pos
    )
  done;
  fun haystack ->
    let len = String.length haystack in
    let p = ref 0 in
    let q = ref 0 in
    while !p < len && !q < needle_len do
      if haystack.[!p] = needle.[!q] then (
        incr p;
        incr q
      ) else if !q = 0 then
        incr p
      else
        q := table.(!q)
    done;
    if !q >= needle_len then
      !p - needle_len
    else
      -1

let is_substring needle =
  let substring_index_memo = substring_index needle in
  (fun haystack -> substring_index_memo haystack >= 0)

(** [lstrip s prefix] returns a copy of [s] with [prefix] removed from
    the beginning if [s] begins with [prefix], or [s] itself if not.
    Physical equality is maintained in the latter case. *)
let lstrip s prefix =
  if string_starts_with s prefix then
    let prefix_length = String.length prefix in
    String.sub s prefix_length (String.length s - prefix_length)
  else
    s

(** [rstrip s suffix] returns a copy of [s] with [suffix] removed from
    the end if [s] ends with [suffix], or [s] itself if not. Physical
    equality is maintained in the latter case. *)
let rstrip s suffix =
  if string_ends_with s suffix then
    let result_length = String.length s - String.length suffix in
    String.sub s 0 result_length
  else
    s

let rpartition s c =
  let sep_idx = String.rindex s c in
  let first = String.sub s 0 sep_idx in
  let second = String.sub s (sep_idx + 1) (String.length s - sep_idx - 1) in
  (first, second)

(** If s is longer than length len, return a copy of s truncated to length len. *)
let truncate len s =
  if String.length s <= len then
    s
  else
    String.sub s 0 len

(** [index_not_from_opt str i chars] is like [index_from_opt], but returns the index of the first
    char in [str] after position [i] that is not in [chars] if it exists, or [None] otherwise. *)
let index_not_from_opt =
  let rec helper i len str chars =
    if i = len then
      None
    else if not (String.contains chars str.[i]) then
      Some i
    else
      helper (i + 1) len str chars
  in
  (fun str i chars -> helper i (String.length str) str chars)

(** [index_not_opt str chars] is like [index_opt], but returns the index of the first char in
    [str] that is not in [chars] if it exists, or [None] otherwise. *)
let index_not_opt str chars = index_not_from_opt str 0 chars

(** [rindex_not_from_opt str i chars] is like [rindex_from_opt], but returns the index of the last
    char in [str] before position [i+1] that is not in [chars] if it exists, or [None] otherwise. *)
let rec rindex_not_from_opt str i chars =
  if i < 0 then
    None
  else if not (String.contains chars str.[i]) then
    Some i
  else
    rindex_not_from_opt str (i - 1) chars

(** [rindex_not_opt str chars] is like [rindex_opt], but returns the index of the last char in
    [str] that is not in [chars] if it exists, or [None] otherwise. *)
let rindex_not_opt str chars = rindex_not_from_opt str (String.length str - 1) chars

let (zero_code, nine_code) = (Char.code '0', Char.code '9')

let is_decimal_digit chr =
  let code = Char.code chr in
  zero_code <= code && code <= nine_code

let is_lowercase_char =
  let (a_code, z_code) = (Char.code 'a', Char.code 'z') in
  fun chr ->
    let code = Char.code chr in
    a_code <= code && code <= z_code

let rec is_not_lowercase str i j =
  if is_lowercase_char str.[i] then
    false
  else if i = j then
    true
  else
    is_not_lowercase str (i + 1) j

(* String provides map and iter but not fold. It also is missing a char_list_of
 * function. Oh well. You can use fold to simulate anything you need, I suppose
 *)
let fold_left ~f ~acc str =
  let acc = ref acc in
  String.iter (fun c -> acc := f !acc c) str;
  !acc

let split c = Str.split (Str.regexp @@ Char.escaped c)

let split2 c s =
  let parts = split c s in
  match parts with
  | [first; second] -> Some (first, second)
  | _ -> None

let split2_exn c s =
  match split2 c s with
  | Some s -> s
  | None -> raise Incorrect_format

(** [replace_char needle replacement str] replaces all instances of the [needle]
    character in [str] with the [replacement] character *)
let replace_char needle replacement =
  String.map (fun c ->
      if c = needle then
        replacement
      else
        c)

(** Splits a string into a list of strings using "\n", "\r" or "\r\n" as
    delimiters. If the string starts or ends with a delimiter, there WILL be an
    empty string at the beginning or end of the list, like [Str.split_delim] does. *)
let split_into_lines str =
  (* To avoid unnecessary string allocations, we're going to keep a list of
   * the start index of each line and how long it is. Then, at the end, we can
   * use String.sub to create the actual strings. *)
  let (_, (last_start, lines)) =
    fold_left
      ~f:(fun (idx, (start, lines)) c ->
        (* For \r\n, we've already processed the newline *)
        if c = '\n' && idx > 0 && str.[idx - 1] = '\r' then
          (idx + 1, (idx + 1, lines))
        else if c = '\n' || c = '\r' then
          (idx + 1, (idx + 1, (start, idx - start) :: lines))
        else
          (idx + 1, (start, lines)))
      ~acc:(0, (0, []))
      str
  in
  (* Reverses the list of start,len and turns them into strings *)
  List.fold_left
    (fun lines (start, len) -> String.sub str start len :: lines)
    []
    ((last_start, String.length str - last_start) :: lines)

(** Splits a string into lines, indents each non-empty line, and concats with newlines *)
let indent indent_size str =
  let padding = String.make indent_size ' ' in
  str
  |> split_into_lines
  |> List.map (fun str ->
         if str = "" then
           ""
         else
           padding ^ str)
  |> String.concat "\n"

(** Splits a string into a list of strings using only "\n" as a delimiter.
    If the string ends with a delimiter, an empty string representing the
    contents after the final delimiter is NOT included (unlike [Str.split_delim]). *)
let split_on_newlines content =
  let re = Str.regexp "[\n]" in
  let lines = Str.split_delim re content in
  (* don't create a list entry for the line after a trailing newline *)
  match List.rev lines with
  | "" :: rest -> List.rev rest
  | _ -> lines

module Internal = struct
  let to_list s =
    let rec loop acc i =
      if i < 0 then
        acc
      else
        (loop [@tailcall]) (s.[i] :: acc) (i - 1)
    in
    loop [] (String.length s - 1)

  let of_list l =
    let s = Bytes.create (List.length l) in
    List.iteri (Bytes.set s) l;
    Bytes.unsafe_to_string s
end

let to_list = Internal.to_list

let of_list = Internal.of_list

module CharSet = struct
  include Set.Make (Char)

  let of_string str = of_list (Internal.to_list str)

  let to_string set = Internal.of_list (elements set)
end

(** Levenshtein distance algorithm.

   Based on the public domain implementation at
   https://bitbucket.org/camlspotter/ocaml_levenshtein/src/default/
*)
let levenshtein_distance (xs : string) (ys : string) =
  let min3 (x : int) y z =
    let m' (a : int) b =
      if a < b then
        a
      else
        b
    in
    m' (m' x y) z
  in
  let cache = Array.init (String.length xs + 1) (fun _ -> Array.make (String.length ys + 1) (-1)) in
  let rec d i j =
    match (i, j) with
    | (0, _) -> j
    | (_, 0) -> i
    | _ ->
      let cache_i = Array.unsafe_get cache i in
      (match Array.unsafe_get cache_i j with
      | -1 ->
        let res =
          let i' = i - 1 in
          let j' = j - 1 in
          min3 (d i' j + 1) (d i j' + 1) (d i' j' + abs (compare xs.[i'] ys.[j']))
        in
        Array.unsafe_set cache_i j res;
        res
      | res -> res)
  in
  d (String.length xs) (String.length ys)
