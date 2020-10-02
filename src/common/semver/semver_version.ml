(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type identifier =
  | Str of string
  | Int of int

type t = {
  major: int;
  minor: int;
  patch: int;
  prerelease: identifier list;
  build: identifier list;
}

let zero = { major = 0; minor = 0; patch = 0; prerelease = []; build = [] }

let compare_identifiers a b =
  match (a, b) with
  | (Int _, Str _) -> -1
  | (Str _, Int _) -> 1
  | (Int a, Int b) -> a - b
  | (Str a, Str b) -> String.compare a b

let compare_identifier_lists =
  let rec compare_sets a b =
    match (a, b) with
    | ([], []) -> 0
    | (_ :: _, []) -> 1 (* a more specific prerelease is greater than one with fewer parts *)
    | ([], _ :: _) -> -1
    | (a_hd :: a_tl, b_hd :: b_tl) ->
      let k = compare_identifiers a_hd b_hd in
      if k <> 0 then
        k
      else
        compare_sets a_tl b_tl
  in
  fun a b ->
    match (a, b) with
    | ([], []) -> 0
    | (_ :: _, []) -> -1 (* being a prerelease is less than not being a prerelease *)
    | ([], _ :: _) -> 1
    | (_, _) -> compare_sets a b

(* Compares the precedence of two versions
 *
 * NOTE: build identifiers are NOT included in precedence! this is the difference vs `compare`
 *
 * From the spec:
 *   Precedence refers to how versions are compared to each other when ordered. Precedence MUST
 *   be calculated by separating the version into major, minor, patch and pre-release identifiers
 *   in that order (Build metadata does not figure into precedence). Precedence is determined by
 *   the first difference when comparing each of these identifiers from left to right as follows:
 *   Major, minor, and patch versions are always compared numerically.
 *   Example: 1.0.0 < 2.0.0 < 2.1.0 < 2.1.1
 *
 *   When major, minor, and patch are equal, a pre-release version has lower precedence than a
 *   normal version. Example: 1.0.0-alpha < 1.0.0. Precedence for two pre-release versions with
 *   the same major, minor, and patch version MUST be determined by comparing each dot separated
 *   identifier from left to right until a difference is found as follows: identifiers consisting
 *   of only digits are compared numerically and identifiers with letters or hyphens are compared
 *   lexically in ASCII sort order. Numeric identifiers always have lower precedence than
 *   non-numeric identifiers. A larger set of pre-release fields has a higher precedence than a
 *   smaller set, if all of the preceding identifiers are equal.
 *   Example: 1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta < 1.0.0-beta.2 <
 *            1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0
 *)
let compare_precedence =
  let compare_ints a b () = a - b in
  let compare_pre a b () = compare_identifier_lists a b in
  let ( >>= ) k f =
    if k <> 0 then
      k
    else
      f ()
  in
  fun { major = a_major; minor = a_minor; patch = a_patch; prerelease = a_pre; build = _ }
      { major = b_major; minor = b_minor; patch = b_patch; prerelease = b_pre; build = _ } ->
    ( 0
      >>= compare_ints a_major b_major
      >>= compare_ints a_minor b_minor
      >>= compare_ints a_patch b_patch
      >>= compare_pre a_pre b_pre
      : int )

let compare a b =
  let k = compare_precedence a b in
  if k <> 0 then
    k
  else
    let { build = a_build; _ } = a in
    let { build = b_build; _ } = b in
    compare_identifier_lists a_build b_build

let incr_major { major; _ } = { zero with major = succ major }

let incr_minor { major; minor; _ } = { zero with major; minor = succ minor }

let incr_patch { major; minor; patch; _ } = { zero with major; minor; patch = succ patch }

let string_of_identifier = function
  | Int x -> string_of_int x
  | Str x -> x

let to_string { major; minor; patch; prerelease; build } =
  let prerelease =
    match prerelease with
    | [] -> ""
    | parts -> "-" ^ (parts |> List.map string_of_identifier |> String.concat ".")
  in
  let build =
    match build with
    | [] -> ""
    | parts -> "+" ^ (parts |> List.map string_of_identifier |> String.concat ".")
  in
  Printf.sprintf "%d.%d.%d%s%s" major minor patch prerelease build
