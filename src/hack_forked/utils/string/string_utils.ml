(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** [lstrip s prefix] returns a copy of [s] with [prefix] removed from
    the beginning if [s] begins with [prefix], or [s] itself if not.
    Physical equality is maintained in the latter case. *)
let lstrip s prefix =
  if String.starts_with ~prefix s then
    let prefix_length = String.length prefix in
    String.sub s prefix_length (String.length s - prefix_length)
  else
    s

(** [rstrip s suffix] returns a copy of [s] with [suffix] removed from
    the end if [s] ends with [suffix], or [s] itself if not. Physical
    equality is maintained in the latter case. *)
let rstrip s suffix =
  if String.ends_with ~suffix s then
    let result_length = String.length s - String.length suffix in
    String.sub s 0 result_length
  else
    s

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

(** [replace_char needle replacement str] replaces all instances of the [needle]
    character in [str] with the [replacement] character *)
let replace_char needle replacement =
  String.map (fun c ->
      if c = needle then
        replacement
      else
        c
  )

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
           padding ^ str
     )
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

(** Escapes special characters to make the given string a valid filename *)
let filename_escape path =
  let buf = Buffer.create (String.length path) in
  String.iter
    (fun ch ->
      match ch with
      | '\\' -> Buffer.add_string buf "zB"
      | ':' -> Buffer.add_string buf "zC"
      | '/' -> Buffer.add_string buf "zS"
      | '\x00' -> Buffer.add_string buf "z0"
      | 'z' -> Buffer.add_string buf "zZ"
      | _ -> Buffer.add_char buf ch)
    path;
  Buffer.contents buf

let filename_unescape str =
  let length = String.length str in
  let buf = Buffer.create length in
  let rec consume i =
    if i >= length then
      ()
    else
      let replacement =
        if i < length - 1 && str.[i] = 'z' then
          match str.[i + 1] with
          | 'B' -> Some '\\'
          | 'C' -> Some ':'
          | 'S' -> Some '/'
          | '0' -> Some '\x00'
          | 'Z' -> Some 'z'
          | _ -> None
        else
          None
      in
      let (c, next_i) =
        match replacement with
        | Some r -> (r, i + 2)
        | None -> (str.[i], i + 1)
      in
      Buffer.add_char buf c;
      consume next_i
  in
  consume 0;
  Buffer.contents buf

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
  include Flow_set.Make (Char)

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
