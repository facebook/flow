(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

exception Parse_error of string

let version_of_string str =
  let lexbuf = Lexing.from_string str in
  try Semver_parser.version Semver_lexer.token lexbuf
  with Parsing.Parse_error -> raise (Parse_error ("Invalid version number: " ^ str))

let range_of_string str =
  let lexbuf = Lexing.from_string str in
  try Semver_parser.range Semver_lexer.token lexbuf
  with Parsing.Parse_error -> raise (Parse_error ("Invalid range: " ^ str))

let is_valid_range range =
  try
    let _ = range_of_string range in
    true
  with Parse_error _ -> false

let satisfies ?include_prereleases (range : string) (version : string) =
  let range = range_of_string range in
  let version = version_of_string version in
  Semver_range.satisfies ?include_prereleases range version

let compare a b = Semver_version.compare_precedence (version_of_string a) (version_of_string b)
