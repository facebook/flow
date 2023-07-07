(*
 * Portions Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*
 * Based on the TypeScript implementation from VSCode:
 * https://github.com/microsoft/vscode/blob/e79a401ba5f6bc8eff07bfffaf4544e96f394837/src/vs/base/test/common/filters.test.ts#L281
 * That implementation is...
 *
 *   Copyright (c) Microsoft Corporation. All rights reserved.
 *)

[@@@ocamlformat "disable"]

let%expect_test "fuzzy_score" =
  let test pattern word =
    let score =
      match Fuzzy_path.fuzzy_score ~pattern word with
      | Some score -> Printf.sprintf "%d" score
      | None -> "none"
    in
    Printf.printf "%S ~> %S: %s\n" pattern word score
  in
  test "a" "a";
  test "A" "A";
  test "A" "a";
  test "a" "A";
  test "a" "ab";
  test "A" "AB";
  test "a" "Ab";
  test "A" "Ab";
  test "ab" "ab";
  test "AB" "AB";
  test "ab" "abc";
  test "AB" "ABC";
  test "ab" "abA";
  test "CONST" "CONST";
  test "CONST" "CxOxNxSxTx";
  test "ABcD" "ABcD";
  test "ccm" "cacmelCase";
  test "bti" "the_black_knight";
  test "ccm" "camelCase";
  test "cmcm" "camelCase";
  test "cmcm" "camelCase";
  test "BK" "the_black_knight";
  test "KeyboardLayout=" "KeyboardLayout";
  test "LLL" "SVisualLoggerLogsList";
  test "LLLL" "SVilLoLosLi";
  test "LLLL" "SVisualLoggerLogsList";
  test "TEdit" "TextEdit";
  test "TEdit" "TextEditor";
  test "TEdit" "Textedit";
  test "TEdit" "text_edit";
  test "TEditDit" "TextEditorDecorationType";
  test "TEdit" "TextEditorDecorationType";
  test "Tedit" "TextEdit";
  test "ba" "?AB?";
  test "bkn" "the_black_knight";
  test "bt" "the_black_knight";
  test "ccm" "camelCasecm";
  test "fdm" "findModel";
  test "fob" "foobar";
  test "fobz" "foobar";
  test "foobar" "foobar";
  test "form" "editor.formatOnSave";
  test "g p" "Git: Pull";
  test "g p" "Git: Pull";
  test "gip" "Git: Pull";
  test "gip" "Git: Pull";
  test "gp" "Git: Pull";
  test "gp" "Git_Git_Pull";
  test "is" "ImportStatement";
  test "is" "isValid";
  test "lowrd" "lowWord";
  test "myvable" "myvariable";
  test "no" "";
  test "no" "match";
  test "ob" "foobar";
  test "sl" "SVisualLoggerLogsList";
  test "sllll" "SVisualLoggerLogsList";
  test "Three" "HTMLHRElement";
  test "Three" "Three";
  test "fo" "barfoo";
  test "fo" "bar_foo";
  test "fo" "bar_Foo";
  test "fo" "bar foo";
  test "fo" "bar.foo";
  test "fo" "bar/foo";
  test "fo" "bar\\foo";
  test "f" "Foo";
  test "tiat" "this_is_a_test";
  test "tiat" "ThisIsATest";
  test "ab" "axxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxbxax";
  test "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
  [%expect {|
    "a" ~> "a": 4
    "A" ~> "A": 4
    "A" ~> "a": 2
    "a" ~> "A": 2
    "a" ~> "ab": 2
    "A" ~> "AB": 2
    "a" ~> "Ab": 0
    "A" ~> "Ab": 2
    "ab" ~> "ab": 12
    "AB" ~> "AB": 12
    "ab" ~> "abc": 10
    "AB" ~> "ABC": 10
    "ab" ~> "abA": 10
    "CONST" ~> "CONST": 36
    "CONST" ~> "CxOxNxSxTx": 14
    "ABcD" ~> "ABcD": 28
    "ccm" ~> "cacmelCase": -1
    "bti" ~> "the_black_knight": none
    "ccm" ~> "camelCase": none
    "cmcm" ~> "camelCase": none
    "cmcm" ~> "camelCase": none
    "BK" ~> "the_black_knight": -10
    "KeyboardLayout=" ~> "KeyboardLayout": none
    "LLL" ~> "SVisualLoggerLogsList": -8
    "LLLL" ~> "SVilLoLosLi": none
    "LLLL" ~> "SVisualLoggerLogsList": none
    "TEdit" ~> "TextEdit": 9
    "TEdit" ~> "TextEditor": 9
    "TEdit" ~> "Textedit": 5
    "TEdit" ~> "text_edit": 4
    "TEditDit" ~> "TextEditorDecorationType": 1
    "TEdit" ~> "TextEditorDecorationType": 9
    "Tedit" ~> "TextEdit": 7
    "ba" ~> "?AB?": none
    "bkn" ~> "the_black_knight": -8
    "bt" ~> "the_black_knight": -21
    "ccm" ~> "camelCasecm": -8
    "fdm" ~> "findModel": 1
    "fob" ~> "foobar": 5
    "fobz" ~> "foobar": none
    "foobar" ~> "foobar": 44
    "form" ~> "editor.formatOnSave": -4
    "g p" ~> "Git: Pull": 4
    "g p" ~> "Git: Pull": 4
    "gip" ~> "Git: Pull": 7
    "gip" ~> "Git: Pull": 7
    "gp" ~> "Git: Pull": -2
    "gp" ~> "Git_Git_Pull": -5
    "is" ~> "ImportStatement": -3
    "is" ~> "isValid": 10
    "lowrd" ~> "lowWord": 14
    "myvable" ~> "myvariable": 23
    "no" ~> "": none
    "no" ~> "match": none
    "ob" ~> "foobar": none
    "sl" ~> "SVisualLoggerLogsList": -4
    "sllll" ~> "SVisualLoggerLogsList": -8
    "Three" ~> "HTMLHRElement": none
    "Three" ~> "Three": 36
    "fo" ~> "barfoo": none
    "fo" ~> "bar_foo": -5
    "fo" ~> "bar_Foo": -5
    "fo" ~> "bar foo": -5
    "fo" ~> "bar.foo": -5
    "fo" ~> "bar/foo": -5
    "fo" ~> "bar\\foo": -5
    "f" ~> "Foo": 0
    "tiat" ~> "this_is_a_test": 1
    "tiat" ~> "ThisIsATest": 1
    "ab" ~> "axxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxbxax": -125
    "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ~> "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx": 1020
  |}]
