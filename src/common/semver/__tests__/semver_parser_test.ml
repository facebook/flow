(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

exception Semver_parse_error of string

let parse_version str =
  let lexbuf = Lexing.from_string str in
  try Semver_parser.version Semver_lexer.token lexbuf with
  | Parsing.Parse_error -> raise (Semver_parse_error (Lexing.lexeme lexbuf))

let parse_comparator str =
  let lexbuf = Lexing.from_string str in
  try Semver_parser.comparator Semver_lexer.token lexbuf with
  | Parsing.Parse_error -> raise (Semver_parse_error (Lexing.lexeme lexbuf))

let parse_range str =
  let lexbuf = Lexing.from_string str in
  try Semver_parser.range Semver_lexer.token lexbuf with
  | Parsing.Parse_error -> raise (Semver_parse_error (Lexing.lexeme lexbuf))

let tests =
  "parser"
  >::: [
         ( "version_basics" >:: fun ctxt ->
           Semver_version.(
             let cases =
               [
                 ("0", zero);
                 ("0.1", { zero with minor = 1 });
                 ("1", { zero with major = 1 });
                 ("1.2", { zero with major = 1; minor = 2 });
                 ("1.2.3", { zero with major = 1; minor = 2; patch = 3 });
                 ( "1.2.3-alpha",
                   { zero with major = 1; minor = 2; patch = 3; prerelease = [Str "alpha"] }
                 );
                 ( "1.2.3-alpha.2",
                   { zero with major = 1; minor = 2; patch = 3; prerelease = [Str "alpha"; Int 2] }
                 );
               ]
             in
             List.iter
               (fun (str, version) ->
                 try assert_equal ~ctxt ~printer:to_string version (parse_version str) with
                 | Semver_parse_error token ->
                   assert_failure ("Failed to parse " ^ str ^ ": unexpected token " ^ token))
               cases;
             assert_bool "done" true
           )
         )
         (* fixes ounit error reporting *);
         ( "comparator_basics" >:: fun ctxt ->
           Semver_comparator.(
             let v1 = Semver_version.{ zero with major = 1 } in
             let cases =
               [
                 (">1", { op = Some Greater; version = v1 });
                 (">=1", { op = Some GreaterOrEqual; version = v1 });
                 ("<1", { op = Some Less; version = v1 });
                 ("<=1", { op = Some LessOrEqual; version = v1 });
                 ("=1", { op = Some Equal; version = v1 });
                 ("1", { op = None; version = v1 });
                 ("= 1", { op = Some Equal; version = v1 });
                 (" = 1", { op = Some Equal; version = v1 });
                 ("  = 1 ", { op = Some Equal; version = v1 });
               ]
             in
             List.iter
               (fun (str, comparator) ->
                 try assert_equal ~ctxt ~printer:to_string comparator (parse_comparator str) with
                 | Semver_parse_error token ->
                   assert_failure ("Failed to parse " ^ str ^ ": unexpected token " ^ token))
               cases;
             assert_bool "done" true
           )
         )
         (* fixes ounit error reporting *);
         ( "range_basics" >:: fun ctxt ->
           Semver_range.(
             let v1 = Semver_version.{ zero with major = 1 } in
             let v2 = Semver_version.{ zero with major = 2 } in
             let ge1 = Comparator Semver_comparator.{ op = Some GreaterOrEqual; version = v1 } in
             let lt2 = Comparator Semver_comparator.{ op = Some Less; version = v2 } in
             let cases =
               [
                 (">=1", [ge1]);
                 (">=1 <2", [ge1; lt2]);
                 ("^1", [Caret v1]);
                 ("^1.0", [Caret v1]);
                 ("^1.0.0", [Caret v1]);
                 ("^1 ^2", [Caret v1; Caret v2]);
                 (">=1 ^2", [ge1; Caret v2]);
               ]
             in
             List.iter
               (fun (str, range) ->
                 try assert_equal ~ctxt ~printer:to_string range (parse_range str) with
                 | Semver_parse_error token ->
                   assert_failure ("Failed to parse " ^ str ^ ": unexpected token " ^ token))
               cases;
             assert_bool "done" true
           )
         );
         (* fixes ounit error reporting *)
       ]
