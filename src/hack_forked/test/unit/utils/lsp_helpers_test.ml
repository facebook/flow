(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Lsp
open Lsp_helpers

let assert_eq (exp : 'a) (actual : 'a) (failure_msg : string) =
  if exp <> actual then failwith failure_msg

let assert_peq (exp : position) (actual : position) (failure_msg : string) =
  if exp <> actual then
    failwith
      (Printf.sprintf
         "%s - expected=(%i:%i) actual=(%i:%i)"
         failure_msg
         exp.line
         exp.character
         actual.line
         actual.character
      )

let assert_req (exp : range option) (actual : range option) (failure_msg : string) =
  match (exp, actual) with
  | (None, Some _) -> failwith (Printf.sprintf "%s - expected None, got Some" failure_msg)
  | (Some _, None) -> failwith (Printf.sprintf "%s - expected Some, got None" failure_msg)
  | (None, None) -> ()
  | (Some exp, Some actual) when exp = actual -> ()
  | (Some exp, Some actual) ->
    failwith
      (Printf.sprintf
         "%s - expected=(%i:%i-%i:%i) actual=(%i:%i-%i:%i)"
         failure_msg
         exp.start.line
         exp.start.character
         exp.end_.line
         exp.end_.character
         actual.start.line
         actual.start.character
         actual.end_.line
         actual.end_.character
      )

let test_pos_compare () =
  let p1 = { line = 1; character = 3 } in
  let p2 = { line = 2; character = 0 } in
  let p3 = { line = 2; character = 1 } in
  assert_eq true (pos_compare p1 p2 < 0) "p1 < p2";
  assert_eq true (pos_compare p2 p1 > 0) "p2 > p1";
  assert_eq true (pos_compare p2 p2 = 0) "p2 = p2";
  assert_eq true (pos_compare p2 p3 < 0) "p2 < p3";
  true

let test_range_overlap () =
  let p1 = { line = 1; character = 5 } in
  let p2 = { line = 2; character = 3 } in
  let p3 = { line = 2; character = 4 } in
  let p4 = { line = 3; character = 1 } in
  let p5 = { line = 3; character = 9 } in
  let p6 = { line = 4; character = 5 } in
  let p7 = { line = 4; character = 8 } in
  let p8 = { line = 5; character = 4 } in
  let sel11 = { start = p1; end_ = p1 } in
  let sel12 = { start = p1; end_ = p2 } in
  let sel13 = { start = p1; end_ = p3 } in
  let sel14 = { start = p1; end_ = p4 } in
  let sel15 = { start = p1; end_ = p5 } in
  let sel16 = { start = p1; end_ = p6 } in
  let sel17 = { start = p1; end_ = p7 } in
  let sel18 = { start = p1; end_ = p8 } in
  let sel22 = { start = p2; end_ = p2 } in
  let sel23 = { start = p2; end_ = p3 } in
  let sel24 = { start = p2; end_ = p4 } in
  let sel25 = { start = p2; end_ = p5 } in
  let sel26 = { start = p2; end_ = p6 } in
  let sel27 = { start = p2; end_ = p7 } in
  let sel28 = { start = p2; end_ = p8 } in
  let sel33 = { start = p3; end_ = p3 } in
  let sel34 = { start = p3; end_ = p4 } in
  let sel35 = { start = p3; end_ = p5 } in
  let sel36 = { start = p3; end_ = p6 } in
  let sel37 = { start = p3; end_ = p7 } in
  let sel38 = { start = p3; end_ = p8 } in
  let sel44 = { start = p4; end_ = p4 } in
  let sel45 = { start = p4; end_ = p5 } in
  let sel46 = { start = p4; end_ = p6 } in
  let sel47 = { start = p4; end_ = p7 } in
  let sel48 = { start = p4; end_ = p8 } in
  let sel55 = { start = p5; end_ = p5 } in
  let sel56 = { start = p5; end_ = p6 } in
  let sel57 = { start = p5; end_ = p7 } in
  let sel58 = { start = p5; end_ = p8 } in
  let sel66 = { start = p6; end_ = p6 } in
  let sel67 = { start = p6; end_ = p7 } in
  let sel68 = { start = p6; end_ = p8 } in
  let sel77 = { start = p7; end_ = p7 } in
  let sel78 = { start = p7; end_ = p8 } in
  let sel88 = { start = p8; end_ = p8 } in
  let squiggle = sel36 in
  assert_eq Selection_before_start_of_squiggle (get_range_overlap sel11 squiggle) "sel11";
  assert_eq Selection_before_start_of_squiggle (get_range_overlap sel12 squiggle) "sel12";
  assert_eq Selection_overlaps_start_of_squiggle (get_range_overlap sel13 squiggle) "sel13";
  assert_eq Selection_overlaps_start_of_squiggle (get_range_overlap sel14 squiggle) "sel14";
  assert_eq Selection_overlaps_start_of_squiggle (get_range_overlap sel15 squiggle) "sel15";
  assert_eq Selection_covers_whole_squiggle (get_range_overlap sel16 squiggle) "sel16";
  assert_eq Selection_covers_whole_squiggle (get_range_overlap sel17 squiggle) "sel17";
  assert_eq Selection_covers_whole_squiggle (get_range_overlap sel18 squiggle) "sel18";
  assert_eq Selection_before_start_of_squiggle (get_range_overlap sel22 squiggle) "sel22";
  assert_eq Selection_overlaps_start_of_squiggle (get_range_overlap sel23 squiggle) "sel23";
  assert_eq Selection_overlaps_start_of_squiggle (get_range_overlap sel24 squiggle) "sel24";
  assert_eq Selection_overlaps_start_of_squiggle (get_range_overlap sel25 squiggle) "sel25";
  assert_eq Selection_covers_whole_squiggle (get_range_overlap sel26 squiggle) "sel26";
  assert_eq Selection_covers_whole_squiggle (get_range_overlap sel27 squiggle) "sel27";
  assert_eq Selection_covers_whole_squiggle (get_range_overlap sel28 squiggle) "sel28";
  assert_eq Selection_overlaps_start_of_squiggle (get_range_overlap sel33 squiggle) "sel33";
  assert_eq Selection_overlaps_start_of_squiggle (get_range_overlap sel34 squiggle) "sel34";
  assert_eq Selection_overlaps_start_of_squiggle (get_range_overlap sel35 squiggle) "sel35";
  assert_eq Selection_covers_whole_squiggle (get_range_overlap sel36 squiggle) "sel36";
  assert_eq Selection_covers_whole_squiggle (get_range_overlap sel37 squiggle) "sel37";
  assert_eq Selection_covers_whole_squiggle (get_range_overlap sel38 squiggle) "sel38";
  assert_eq Selection_in_middle_of_squiggle (get_range_overlap sel44 squiggle) "sel44";
  assert_eq Selection_in_middle_of_squiggle (get_range_overlap sel45 squiggle) "sel45";
  assert_eq Selection_overlaps_end_of_squiggle (get_range_overlap sel46 squiggle) "sel46";
  assert_eq Selection_overlaps_end_of_squiggle (get_range_overlap sel47 squiggle) "sel47";
  assert_eq Selection_overlaps_end_of_squiggle (get_range_overlap sel48 squiggle) "sel48";
  assert_eq Selection_in_middle_of_squiggle (get_range_overlap sel55 squiggle) "sel55";
  assert_eq Selection_overlaps_end_of_squiggle (get_range_overlap sel56 squiggle) "sel56";
  assert_eq Selection_overlaps_end_of_squiggle (get_range_overlap sel57 squiggle) "sel57";
  assert_eq Selection_overlaps_end_of_squiggle (get_range_overlap sel58 squiggle) "sel58";
  assert_eq Selection_overlaps_end_of_squiggle (get_range_overlap sel66 squiggle) "sel66";
  assert_eq Selection_overlaps_end_of_squiggle (get_range_overlap sel67 squiggle) "sel67";
  assert_eq Selection_overlaps_end_of_squiggle (get_range_overlap sel68 squiggle) "sel68";
  assert_eq Selection_after_end_of_squiggle (get_range_overlap sel77 squiggle) "sel77";
  assert_eq Selection_after_end_of_squiggle (get_range_overlap sel78 squiggle) "sel78";
  assert_eq Selection_after_end_of_squiggle (get_range_overlap sel88 squiggle) "sel88";
  true

let test_update_pos_due_to_prior_replace () =
  let remove_many = { start = { line = 1; character = 6 }; end_ = { line = 3; character = 3 } } in
  let remove_line = { start = { line = 1; character = 3 }; end_ = { line = 1; character = 6 } } in
  let m_m = { remove_range = remove_many; insert_lines = 3; insert_chars_on_final_line = 4 } in
  let m_l = { remove_range = remove_many; insert_lines = 0; insert_chars_on_final_line = 4 } in
  let l_m = { remove_range = remove_line; insert_lines = 3; insert_chars_on_final_line = 4 } in
  let l_l = { remove_range = remove_line; insert_lines = 0; insert_chars_on_final_line = 4 } in
  let p1 = { line = 1; character = 11 } in
  let p3 = { line = 3; character = 8 } in
  let p5 = { line = 5; character = 1 } in
  assert_peq
    { line = 4; character = 4 }
    (update_pos_due_to_prior_replace remove_many.end_ m_m)
    "replace multiline->multiline, point at end of removal";
  assert_peq
    { line = 4; character = 9 }
    (update_pos_due_to_prior_replace p3 m_m)
    "replace multiline->multiline, point on same line";
  assert_peq
    { line = 6; character = 1 }
    (update_pos_due_to_prior_replace p5 m_m)
    "replace multiline->multiline, point on later line";
  assert_peq
    { line = 1; character = 10 }
    (update_pos_due_to_prior_replace remove_many.end_ m_l)
    "replace multiline->one line, point at end of removal";
  assert_peq
    { line = 1; character = 15 }
    (update_pos_due_to_prior_replace p3 m_l)
    "replace multiline->one line, point on same line";
  assert_peq
    { line = 3; character = 1 }
    (update_pos_due_to_prior_replace p5 m_l)
    "replace multiline->one line, point on later line";
  assert_peq
    { line = 4; character = 4 }
    (update_pos_due_to_prior_replace remove_line.end_ l_m)
    "replace one line->multiline, point at end of removal";
  assert_peq
    { line = 4; character = 9 }
    (update_pos_due_to_prior_replace p1 l_m)
    "replace one line->multiline, point on same line";
  assert_peq
    { line = 6; character = 8 }
    (update_pos_due_to_prior_replace p3 l_m)
    "replace one line->multiline, point on later line";
  assert_peq
    { line = 1; character = 7 }
    (update_pos_due_to_prior_replace remove_line.end_ l_l)
    "replace one line->one line, point at end of removal";
  assert_peq
    { line = 1; character = 12 }
    (update_pos_due_to_prior_replace p1 l_l)
    "replace one line->one line, point on same line";
  assert_peq
    { line = 3; character = 8 }
    (update_pos_due_to_prior_replace p3 l_l)
    "replace one line->one line, point on later line";
  true

let test_update_range_due_to_replace () =
  let squiggle = { start = { line = 2; character = 3 }; end_ = { line = 2; character = 8 } } in
  let remove_range = { start = { line = 0; character = 0 }; end_ = { line = 1; character = 0 } } in
  let replace = { remove_range; insert_lines = 0; insert_chars_on_final_line = 0 } in
  let exp = { start = { line = 1; character = 3 }; end_ = { line = 1; character = 8 } } in
  assert_req (Some exp) (update_range_due_to_replace squiggle replace) "delete line above squiggle";

  let remove_range = { start = { line = 0; character = 0 }; end_ = { line = 0; character = 0 } } in
  let replace = { remove_range; insert_lines = 1; insert_chars_on_final_line = 0 } in
  let exp = { start = { line = 3; character = 3 }; end_ = { line = 3; character = 8 } } in
  assert_req (Some exp) (update_range_due_to_replace squiggle replace) "add line above squiggle";

  let remove_range = { start = { line = 2; character = 2 }; end_ = { line = 2; character = 3 } } in
  let replace = { remove_range; insert_lines = 0; insert_chars_on_final_line = 0 } in
  let exp = { start = { line = 2; character = 2 }; end_ = { line = 2; character = 7 } } in
  assert_req (Some exp) (update_range_due_to_replace squiggle replace) "del char before squiggle";

  let remove_range = { start = { line = 2; character = 3 }; end_ = { line = 2; character = 3 } } in
  let replace = { remove_range; insert_lines = 0; insert_chars_on_final_line = 1 } in
  let exp = { start = { line = 2; character = 4 }; end_ = { line = 2; character = 9 } } in
  assert_req (Some exp) (update_range_due_to_replace squiggle replace) "add char before squiggle";

  let remove_range = { start = { line = 2; character = 5 }; end_ = { line = 2; character = 6 } } in
  let replace = { remove_range; insert_lines = 0; insert_chars_on_final_line = 0 } in
  let exp = { start = { line = 2; character = 3 }; end_ = { line = 2; character = 7 } } in
  assert_req (Some exp) (update_range_due_to_replace squiggle replace) "del char in squiggle";

  let remove_range = { start = { line = 2; character = 5 }; end_ = { line = 2; character = 5 } } in
  let replace = { remove_range; insert_lines = 0; insert_chars_on_final_line = 1 } in
  let exp = { start = { line = 2; character = 3 }; end_ = { line = 2; character = 9 } } in
  assert_req (Some exp) (update_range_due_to_replace squiggle replace) "add char in squiggle";

  let remove_range = { start = { line = 2; character = 8 }; end_ = { line = 3; character = 0 } } in
  let replace = { remove_range; insert_lines = 0; insert_chars_on_final_line = 0 } in
  let exp = { start = { line = 2; character = 3 }; end_ = { line = 2; character = 8 } } in
  assert_req (Some exp) (update_range_due_to_replace squiggle replace) "del line after squiggle";

  let remove_range = { start = { line = 2; character = 8 }; end_ = { line = 2; character = 8 } } in
  let replace = { remove_range; insert_lines = 1; insert_chars_on_final_line = 0 } in
  let exp = { start = { line = 2; character = 3 }; end_ = { line = 2; character = 8 } } in
  assert_req (Some exp) (update_range_due_to_replace squiggle replace) "add line after squiggle";

  let remove_range = { start = { line = 2; character = 8 }; end_ = { line = 2; character = 9 } } in
  let replace = { remove_range; insert_lines = 0; insert_chars_on_final_line = 0 } in
  let exp = { start = { line = 2; character = 3 }; end_ = { line = 2; character = 8 } } in
  assert_req (Some exp) (update_range_due_to_replace squiggle replace) "del char after squiggle";

  let remove_range = { start = { line = 2; character = 8 }; end_ = { line = 2; character = 8 } } in
  let replace = { remove_range; insert_lines = 0; insert_chars_on_final_line = 1 } in
  let exp = { start = { line = 2; character = 3 }; end_ = { line = 2; character = 8 } } in
  assert_req (Some exp) (update_range_due_to_replace squiggle replace) "add char after squiggle";

  let remove_range = { start = { line = 2; character = 3 }; end_ = { line = 2; character = 8 } } in
  let replace = { remove_range; insert_lines = 0; insert_chars_on_final_line = 0 } in
  assert_req None (update_range_due_to_replace squiggle replace) "del squiggle";

  let remove_range = { start = { line = 2; character = 0 }; end_ = { line = 3; character = 0 } } in
  let replace = { remove_range; insert_lines = 0; insert_chars_on_final_line = 0 } in
  assert_req None (update_range_due_to_replace squiggle replace) "del squiggle line";

  let remove_range = { start = { line = 2; character = 0 }; end_ = { line = 3; character = 0 } } in
  let replace = { remove_range; insert_lines = 1; insert_chars_on_final_line = 10 } in
  assert_req None (update_range_due_to_replace squiggle replace) "replace squiggle line";

  let remove_range = { start = { line = 2; character = 0 }; end_ = { line = 2; character = 5 } } in
  let replace = { remove_range; insert_lines = 0; insert_chars_on_final_line = 1 } in
  let exp = { start = { line = 2; character = 1 }; end_ = { line = 2; character = 4 } } in
  assert_req (Some exp) (update_range_due_to_replace squiggle replace) "replace squiggle start";

  let remove_range = { start = { line = 2; character = 5 }; end_ = { line = 2; character = 12 } } in
  let replace = { remove_range; insert_lines = 0; insert_chars_on_final_line = 4 } in
  let exp = { start = { line = 2; character = 3 }; end_ = { line = 2; character = 5 } } in
  assert_req (Some exp) (update_range_due_to_replace squiggle replace) "replace squiggle end";
  true

let tests =
  [
    ("test_pos_compare", test_pos_compare);
    ("test_range_overlap", test_range_overlap);
    ("test_update_pos_due_to_prior_replace", test_update_pos_due_to_prior_replace);
    ("test_update_range_due_to_replace", test_update_range_due_to_replace);
  ]

let () = Unit_test.run_all tests
