(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 * This library contains portions based on third party software provided under
 * this license:
 *
 * node-semver software
 *
 * Copyright (c) Isaac Z. Schlueter and Contributors
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
 * IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(**
  Basic semantic version parser, as defined by http://semver.org/

  So far, this implementation only supports individual versions; intersection
  ranges (e.g. ">=0.13.0 <0.14.0", which are ANDed together); and caret ranges,
  which allow changes that do not modify the left-most non-zero digit (e.g.
  "^0.13" expands into ">=0.13.0 <0.14.0", and "^0.13.1" expands into
  ">=0.13.1 <0.14.0", whereas "^1.2.3" expands into ">=1.2.3 <2.0.0").

  Further support for features like "||" ("1.2.3 || 1.2.5"), hyphen ranges
  ("1.2 - 1.3"), X-ranges ("1.2.x" or "1.2.*"), tilde ranges ("~1.2"), and
  pre-release/build identifiers ("1.2.3-beta.1"), will be added as necessary.
 **)

open Utils_js

exception Parse_error of string

let numeric_identifier_str = "0\\|[1-9][0-9]*"
let version_str = spf "\\(%s\\)\\(\\.\\(%s\\)\\(\\.\\(%s\\)\\)?\\)?"
  numeric_identifier_str numeric_identifier_str numeric_identifier_str
let version_regexp = Str.regexp version_str

let get_matched_version s offset =
  let get_match index =
    try Str.matched_group (offset + index) s with Not_found -> ""
  in
  get_match 1, get_match 3, get_match 5

let missing id = id = ""

module Version : sig
  type t
  val create : int -> int -> int -> t
  val parse : string -> t
  val compare : t -> t -> int
end = struct
  type t = {
    major: int;
    minor: int;
    patch: int;
  }

  let create major minor patch = { major; minor; patch; }

  let parse version = try
    assert (Str.string_match version_regexp version 0);
    let major, minor, patch = get_matched_version version 0 in
    let major = if missing major then 0 else int_of_string major in
    let minor = if missing minor then 0 else int_of_string minor in
    let patch = if missing patch then 0 else int_of_string patch in
    create major minor patch
  with _ ->
    raise (Parse_error ("Invalid version number: " ^ version))

  let compare a b =
    Pervasives.compare (a.major, a.minor, a.patch) (b.major, b.minor, b.patch)
end

module Comparator : sig
  type t
  val parse : string -> t
  val satisfies : Version.t -> t -> bool
end = struct
  type op =
  | Greater
  | GreaterOrEqual
  | Less
  | LessOrEqual
  | Equal
  | NotEqual

  type t = {
    op: op;
    version: Version.t
  }

  let comparator_regexp = Str.regexp (
    spf "^\\([<>=]?=?\\|!=\\)\\(%s\\)$" version_str
  )

  let parse comparator =
    try
      assert (Str.string_match comparator_regexp comparator 0);
      let op = match Str.matched_group 1 comparator with
      | ">" -> Greater
      | ">=" -> GreaterOrEqual
      | "<" -> Less
      | "<=" -> LessOrEqual
      | "!=" -> NotEqual
      | "" | "=" | "==" -> Equal
      | other -> raise (Parse_error ("Unknown operator: " ^ other))
      in

      let major, minor, patch = get_matched_version comparator 2 in
      let major = if missing major then 0 else int_of_string major in
      let minor = if missing minor then 0 else int_of_string minor in
      let patch = if missing patch then 0 else int_of_string patch in
      let version = Version.create major minor patch in
      {op; version}
    with _ ->
      raise (Parse_error ("Invalid comparator: " ^ comparator))

  let satisfies a {op; version = b} =
    let result = Version.compare a b in
    match op with
    | Greater -> result > 0
    | GreaterOrEqual -> result >= 0
    | Less -> result < 0
    | LessOrEqual -> result <= 0
    | NotEqual -> result <> 0
    | Equal -> result = 0
end

module Range : sig
  type t
  val parse : string -> t
  val satisfies : t -> Version.t -> bool
end = struct
  type t = Comparator.t list

  let op_trim = Str.regexp "\\([<>]=?\\|[!=]?=\\|\\^\\)[ \t]+"
  let caret_regexp = Str.regexp (spf "^\\^\\(%s\\)$" version_str)

  let incr str = (int_of_string str) + 1

  let expand_caret ver =
    let major, minor, patch = ver in
    if missing major then ""
    else if missing minor then spf ">=%s.0.0 <%d.0.0" major (incr major)
    else if missing patch then
      if major = "0" then
        spf ">=%s.%s.0 <%s.%d.0" major minor major (incr minor)
      else
        spf ">=%s.%s.0 <%d.0.0" major minor (incr major)
    else
      if major = "0" then
        if minor = "0" then
          spf ">=%s.%s.%s <%s.%s.%d" major minor patch major minor (incr patch)
        else
          spf ">=%s.%s.%s <%s.%d.0" major minor patch major (incr minor)
      else
        spf ">=%s.%s.%s <%d.0.0" major minor patch (incr major)

  let expand_comparator (comp:string) : string list =
    let comp =
      if Str.string_match caret_regexp comp 0 then
        expand_caret (get_matched_version comp 1)
      else comp
    in
    Str.split (Str.regexp_string " ") comp

  let parse (range:string) : t =
    (* normalize whitespace *)
    let range = String.trim range in
    let range = Str.global_replace op_trim "\\1" range in
    let range = Str.global_replace (Str.regexp "[ \t]+") " " range in

    range
    |> Str.split (Str.regexp_string " ")
    |> List.map expand_comparator
    |> List.flatten
    |> List.map Comparator.parse

  let satisfies range version =
    List.for_all (Comparator.satisfies version) range
end

let is_valid_range range =
  try let _ = Range.parse range in true
  with Parse_error _ -> false

let satisfies (range:string) (version:string) =
  let range = Range.parse range in
  let version = Version.parse version in
  Range.satisfies range version
