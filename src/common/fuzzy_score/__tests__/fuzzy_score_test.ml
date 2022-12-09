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

let compare (a_score, a_len, a) (b_score, b_len, b) =
  match Base.Int.compare a_score b_score with
  | 0 ->
    (match Base.Int.compare a_len b_len with
    | 0 -> String.compare a b
    | k -> k)
  | k -> k

let test pattern words =
  Base.List.map words ~f:(fun word ->
      let score = Fuzzy_score.fuzzy_score ~pattern word in
      (score, String.length word, word)
  )
  |> Base.List.sort ~compare
  |> Base.List.iter ~f:(fun (score, _, word) ->
         Printf.printf "%-20s: %3d\n" ("\"" ^ word ^ "\"") score
     )

[@@@ocamlformat "disable"]

let%expect_test "fuzzyScore" =
  test "ab" ["abA"];
  test "ccm" ["cacmelCase"];
  test "bti" ["the_black_knight"];
  test "ccm" ["camelCase"];
  test "cmcm" ["camelCase"];
  test "cmcm" ["camelCase"];
  test "BK" ["the_black_knight"];
  test "KeyboardLayout=" ["KeyboardLayout"];
  test "LLL" ["SVisualLoggerLogsList"];
  test "LLLL" ["SVilLoLosLi"];
  test "LLLL" ["SVisualLoggerLogsList"];
  test "TEdit" ["TextEdit"];
  test "TEdit" ["TextEditor"];
  test "TEdit" ["Textedit"];
  test "TEdit" ["text_edit"];
  test "TEditDit" ["TextEditorDecorationType"];
  test "TEdit" ["TextEditorDecorationType"];
  test "Tedit" ["TextEdit"];
  test "ba" ["?AB?"];
  test "bkn" ["the_black_knight"];
  test "bt" ["the_black_knight"];
  test "ccm" ["camelCasecm"];
  test "fdm" ["findModel"];
  test "fob" ["foobar"];
  test "fobz" ["foobar"];
  test "foobar" ["foobar"];
  test "form" ["editor.formatOnSave"];
  test "g p" ["Git: Pull"];
  test "g p" ["Git: Pull"];
  test "gip" ["Git: Pull"];
  test "gip" ["Git: Pull"];
  test "gp" ["Git: Pull"];
  test "gp" ["Git_Git_Pull"];
  test "is" ["ImportStatement"];
  test "is" ["isValid"];
  test "lowrd" ["lowWord"];
  test "myvable" ["myvariable"];
  test "no" [""];
  test "no" ["match"];
  test "ob" ["foobar"];
  test "sl" ["SVisualLoggerLogsList"];
  test "sllll" ["SVisualLoggerLogsList"];
  test "Three" ["HTMLHRElement"];
  test "Three" ["Three"];
  test "fo" ["barfoo"];
  test "fo" ["bar_foo"];
  test "fo" ["bar_Foo"];
  test "fo" ["bar foo"];
  test "fo" ["bar.foo"];
  test "fo" ["bar/foo"];
  test "fo" ["bar\\foo"];
  [%expect {|
    "abA"               :  10
    "cacmelCase"        :  -1
    "the_black_knight"  :   0
    "camelCase"         :   0
    "camelCase"         :   0
    "camelCase"         :   0
    "the_black_knight"  : -10
    "KeyboardLayout"    :   0
    "SVisualLoggerLogsList":  -8
    "SVilLoLosLi"       :   0
    "SVisualLoggerLogsList":   0
    "TextEdit"          :   9
    "TextEditor"        :   9
    "Textedit"          :   5
    "text_edit"         :   4
    "TextEditorDecorationType":   1
    "TextEditorDecorationType":   9
    "TextEdit"          :   7
    "?AB?"              :   0
    "the_black_knight"  :  -8
    "the_black_knight"  : -21
    "camelCasecm"       :  -8
    "findModel"         :   1
    "foobar"            :   5
    "foobar"            :   0
    "foobar"            :  44
    "editor.formatOnSave":  -4
    "Git: Pull"         :   4
    "Git: Pull"         :   4
    "Git: Pull"         :   7
    "Git: Pull"         :   7
    "Git: Pull"         :  -2
    "Git_Git_Pull"      :  -5
    "ImportStatement"   :  -3
    "isValid"           :  10
    "lowWord"           :  14
    "myvariable"        :  23
    ""                  :   0
    "match"             :   0
    "foobar"            :   0
    "SVisualLoggerLogsList":  -4
    "SVisualLoggerLogsList":  -8
    "HTMLHRElement"     :   0
    "Three"             :  36
    "barfoo"            :   0
    "bar_foo"           :  -5
    "bar_Foo"           :  -5
    "bar foo"           :  -5
    "bar.foo"           :  -5
    "bar/foo"           :  -5
    "bar\foo"           :  -5
  |}]
