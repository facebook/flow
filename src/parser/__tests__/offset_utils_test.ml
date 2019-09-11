(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let pos line column = Loc.{ line; column }

(* UTF-8 encoding of code point 0x2028, line separator *)
let line_sep = "\xe2\x80\xa8"

(* UTF-8 encoding of code point 0x2029, paragraph separator *)
let par_sep = "\xe2\x80\xa9"

(* UTF-8 encoding of code point 0x1f603, some form of a smiley *)
let smiley = "\xf0\x9f\x98\x83"

let str_with_smiley = Printf.sprintf "foo %s bar\nbaz\n" smiley

let get_offset table pos =
  try Offset_utils.offset table pos
  with Offset_utils.Offset_lookup_failed (_pos, msg) ->
    assert_failure
      (Printf.sprintf "Lookup failed: %s\nTable:\n%s" msg (Offset_utils.debug_string table))

let run ctxt text (line, col) expected_offset =
  let table = Offset_utils.make text in
  let offset = get_offset table (pos line col) in
  assert_equal ~ctxt ~printer:string_of_int expected_offset offset

let run_expect_failure text (line, col) expected_msg =
  let table = Offset_utils.make text in
  let p = pos line col in
  let f () = Offset_utils.offset table p in
  let expected_exn = Offset_utils.Offset_lookup_failed (p, expected_msg) in
  assert_raises expected_exn f

class loc_extractor =
  object (this)
    inherit [Loc.t, Loc.t, unit, unit] Flow_polymorphic_ast_mapper.mapper

    (* Locations built up in reverse order *)
    val mutable locs = []

    method get_locs = locs

    method on_loc_annot loc = locs <- loc :: locs

    method on_type_annot = this#on_loc_annot
  end

let extract_locs ast =
  let extractor = new loc_extractor in
  let (_ : (unit, unit) Flow_ast.program) = extractor#program ast in
  List.rev extractor#get_locs

(* This tests to make sure that we can find an offset for all real-world locations that the parser
 * can produce, and that I haven't made any incorrect assumptions about edge cases in the rest of
 * the tests. *)
let run_full_test source =
  let (ast, _) = Parser_flow.program ~fail:false source in
  let all_locs = extract_locs ast in
  let all_positions =
    Loc.(
      let all_starts = List.map (fun { start; _ } -> start) all_locs in
      let all_ends = List.map (fun { _end; _ } -> _end) all_locs in
      all_starts @ all_ends)
  in
  let offset_table = Offset_utils.make source in
  (* Just make sure it doesn't crash *)
  List.iter
    begin
      fun loc ->
      let (_ : int) = get_offset offset_table loc in
      ()
    end
    all_positions

let tests =
  "offset_utils"
  >::: [
         ("empty_line" >:: (fun ctxt -> run ctxt "foo\n\nbar" (3, 0) 5));
         ( "Loc.none"
         >:: fun ctxt ->
         (* This is a fake location but it's used often enough that we should at least not crash when
          * encountering it. *)
         run ctxt "" (0, 0) 0 );
         ("first_char" >:: (fun ctxt -> run ctxt "foo bar\n" (1, 0) 0));
         ("last_char" >:: (fun ctxt -> run ctxt "foo bar\n" (1, 6) 6));
         ( "column_after_last"
         >:: fun ctxt ->
         (* The parser gives us locations where the `end` position is exclusive. Even though the last
          * character of the "foo" token is in column 2, the location of "foo" is given as
          * ((1, 0), (1, 3)). Because of this, we need to make sure we can look up locations that are
          * after the final column of a line, even though these locations don't correspond with an actual
          * character. *)
         run ctxt "foo\nbar\n" (1, 3) 3 );
         ( "char_after_last"
         >:: fun ctxt ->
         (* See the comment in the previous test *)
         run ctxt "foo\nbar" (2, 3) 7 );
         ( "empty"
         >:: fun ctxt ->
         (* Similar to above, we should be able to get one offset in an empty string *)
         run ctxt "" (1, 0) 0 );
         ("no_last_line_terminator" >:: (fun ctxt -> run ctxt "foo bar" (1, 6) 6));
         ("multi_line" >:: (fun ctxt -> run ctxt "foo\nbar\n" (2, 1) 5));
         ("carriage_return" >:: (fun ctxt -> run ctxt "foo\rbar\r" (2, 1) 5));
         ("windows_line_terminator" >:: (fun ctxt -> run ctxt "foo\r\nbar\r\n" (2, 1) 6));
         ( "unicode_line_separator"
         >:: fun ctxt ->
         (* Each line separator character is 3 bytes. The returned offset reflects that. *)
         run ctxt (Printf.sprintf "foo%sbar%s" line_sep line_sep) (2, 1) 7 );
         ( "unicode_paragraph_separator"
         >:: fun ctxt ->
         (* Each line separator character is 3 bytes. The returned offset reflects that. *)
         run ctxt (Printf.sprintf "foo%sbar%s" par_sep par_sep) (2, 1) 7 );
         ("offset_before_multibyte_char" >:: (fun ctxt -> run ctxt str_with_smiley (1, 3) 3));
         ( "offset_of_multibyte_char"
         >:: fun ctxt ->
         (* This is the position of the smiley. The offset should give us the first byte in the
          * character. *)
         run ctxt str_with_smiley (1, 4) 4 );
         ( "offset_after_multibyte_char"
         >:: fun ctxt ->
         (* This is the position after the smiley. The offset should reflect the width of the multibyte
          * character (4 bytes in this case). *)
         run ctxt str_with_smiley (1, 5) 8 );
         ("offset_line_after_multibyte_char" >:: (fun ctxt -> run ctxt str_with_smiley (2, 0) 13));
         ( "out_of_bounds_line"
         >:: fun _ctxt ->
         run_expect_failure "foo\n" (5, 0) "Failure while looking up line. Index: 4. Length: 2." );
         ( "out_of_bounds_column"
         >:: fun _ctxt ->
         run_expect_failure
           "foo\n"
           (1, 10)
           "Failure while looking up column. Index: 10. Length: 4." );
         ( "full_test"
         >:: fun _ctxt ->
         (* Note that there is no newline at the end of the string -- I found a bug in an initial version
          * which was exposed by not having a final newline character. *)
         let source = "const foo = 4;\nconst bar = foo + 2;" in
         run_full_test source );
         ( "lexing_error_newline_test"
         >:: fun _ctxt ->
         let source = "\"foo\nbar\"" in
         run_full_test source );
         ( "lexing_error_throw"
         >:: fun _ctxt ->
         let source = "throw\n" in
         run_full_test source );
         ( "lexing_error_regex_newline"
         >:: fun _ctxt ->
         let source = "/\n/" in
         run_full_test source );
         ( "lexing_error_complex_regex_newline"
         >:: fun _ctxt ->
         let source = "/a\\\n/" in
         run_full_test source );
       ]
