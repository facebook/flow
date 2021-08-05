(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Layout
open OUnit2

let space_regex = Str.regexp_string " "

let flat_pretty_space = IfBreak (Empty, pretty_space)

let assert_pretty_print ~ctxt ?msg expected_str layout =
  let out = layout |> Pretty_printer.print ~source_maps:None |> Source.contents |> String.trim in
  let printer x = Str.global_replace space_regex "\xE2\x90\xA3" x (* open box *) in
  assert_equal ~ctxt ?msg ~printer expected_str out

let tests =
  "pretty_printer"
  >::: [
         ( "breaks_in_list" >:: fun ctxt ->
           let layout =
             fuse
               [
                 Atom "return";
                 space;
                 list ~wrap:(IfBreak (Atom "(", Empty), IfBreak (Atom ")", Empty)) [Atom "null"];
               ]
           in
           assert_pretty_print ~ctxt "return null" layout;

           let long_string = String.make 80 'x' in
           let layout =
             fuse
               [
                 Atom "return";
                 space;
                 list
                   ~wrap:(IfBreak (Atom "(", Empty), IfBreak (Atom ")", Empty))
                   [Atom long_string];
               ]
           in
           assert_pretty_print ~ctxt ("return (\n  " ^ long_string ^ "\n)") layout );
         ( "force_breaks_in_list" >:: fun ctxt ->
           let short_string = String.make 10 'x' in
           let layout =
             fuse
               [
                 Atom "myFunc";
                 list
                   ~wrap:(Atom "(", Atom ")")
                   ~sep:(Atom ",")
                   [
                     Atom "a";
                     fuse
                       [
                         Atom "b";
                         space;
                         Atom "=>";
                         Indent (fuse [pretty_hardline; Atom short_string]);
                       ];
                   ];
               ]
           in
           assert_pretty_print ~ctxt ("myFunc(\n  a,\n  b =>\n    " ^ short_string ^ ",\n)") layout
         );
         ( "sequence_inline_after" >:: fun ctxt ->
           let short_string = String.make 10 'x' in
           let long_string = String.make 80 'x' in
           begin
             let layout =
               fuse
                 [
                   Atom short_string;
                   Sequence
                     ( { break = Break_if_needed; inline = (false, true); indent = 2 },
                       [fuse [flat_pretty_space; Atom short_string]] );
                 ]
             in
             assert_pretty_print ~ctxt (short_string ^ " " ^ short_string) layout
           end;

           begin
             let layout =
               fuse
                 [
                   fuse
                     [
                       Atom short_string;
                       Sequence
                         ( { break = Break_if_needed; inline = (false, true); indent = 2 },
                           [fuse [flat_pretty_space; Atom short_string]] );
                     ];
                   Sequence
                     ( { break = Break_if_needed; inline = (false, true); indent = 2 },
                       [fuse [flat_pretty_space; Atom short_string]] );
                 ]
             in
             assert_pretty_print
               ~ctxt
               (short_string ^ " " ^ short_string ^ " " ^ short_string)
               layout
           end;

           begin
             let layout =
               fuse
                 [
                   Atom long_string;
                   Sequence
                     ( { break = Break_if_needed; inline = (false, true); indent = 2 },
                       [fuse [flat_pretty_space; Atom long_string]] );
                 ]
             in
             assert_pretty_print ~ctxt (long_string ^ "\n  " ^ long_string) layout
           end;

           let layout =
             fuse
               [
                 fuse
                   [
                     Atom long_string;
                     Sequence
                       ( { break = Break_if_needed; inline = (false, true); indent = 2 },
                         [fuse [flat_pretty_space; Atom long_string]] );
                   ];
                 Sequence
                   ( { break = Break_if_needed; inline = (false, true); indent = 2 },
                     [fuse [flat_pretty_space; Atom long_string]] );
               ]
           in
           assert_pretty_print
             ~ctxt
             (long_string ^ "\n  " ^ long_string ^ "\n  " ^ long_string)
             layout );
         ( "if_break_inside_concat_inside_sequence" >:: fun ctxt ->
           let a40 = String.make 40 'A' in
           let layout =
             Sequence
               ( { break = Break_if_needed; inline = (true, true); indent = 0 },
                 [Concat [Atom a40; IfBreak (Empty, Atom " "); Atom a40]] )
           in
           (* the IfBreak generates Empty because a break is needed, but no newline because it's fused *)
           assert_pretty_print ~ctxt (a40 ^ a40) layout;

           let layout =
             Sequence
               ( { break = Break_if_needed; inline = (true, true); indent = 0 },
                 [Concat [Atom a40; IfBreak (Empty, Atom " "); Atom a40]; Atom a40] )
           in
           (* same as above. the Concat would be 81 chars if it doesn't break, which causes the parent
              Sequence to break, so the IfBreak takes the "break" case instead, and there's a NL between
              the Concat and last Atom. *)
           assert_pretty_print ~ctxt (a40 ^ a40 ^ "\n" ^ a40) layout );
         ( "break_if_needed_sequence_inside_concat" >:: fun ctxt ->
           let a80 = String.make 80 'A' in
           (* fits in 80 cols *)
           let layout =
             Concat
               [
                 Atom "(";
                 Sequence
                   ({ break = Break_if_needed; inline = (false, false); indent = 2 }, [Atom "a"]);
                 Atom ")";
               ]
           in
           assert_pretty_print ~ctxt "(a)" layout;

           (* doesn't fit in 80 cols, so indents *)
           let layout =
             Concat
               [
                 Atom "(";
                 Sequence
                   ({ break = Break_if_needed; inline = (false, false); indent = 2 }, [Atom a80]);
                 Atom ")";
               ]
           in
           assert_pretty_print ~ctxt ("(\n  " ^ a80 ^ "\n)") layout;

           (* doesn't fit in 80 cols, but doesn't indent *)
           let layout =
             Concat
               [
                 Atom "(";
                 Sequence
                   ({ break = Break_if_needed; inline = (true, true); indent = 2 }, [Atom a80]);
                 Atom ")";
               ]
           in
           assert_pretty_print ~ctxt ("(" ^ a80 ^ ")") layout );
         ( "group_break" >:: fun ctxt ->
           let a40 = String.make 40 'A' in
           let a80 = String.make 80 'A' in
           (* fits *)
           assert_pretty_print ~ctxt ("(" ^ a40 ^ ")") (Group [Atom "("; Atom a40; Atom ")"]);

           (* fits *)
           assert_pretty_print
             ~ctxt
             ("( " ^ a40 ^ " )")
             (Group [Atom "("; line; Atom a40; line; Atom ")"]);

           (* exceeds 80 cols since there are no breaks *)
           assert_pretty_print ~ctxt ("(" ^ a80 ^ ")") (Group [Atom "("; Atom a80; Atom ")"]);

           (* breaks *)
           assert_pretty_print
             ~ctxt
             ("(\n" ^ a80 ^ "\n)")
             (Group [Atom "("; line; Atom a80; line; Atom ")"]);

           (* doesn't exceed 80 cols *)
           assert_pretty_print
             ~ctxt
             ("( " ^ a40 ^ " )")
             (Group [Atom "("; Indent (Concat [line; Atom a40]); line; Atom ")"]);

           (* breaks, but indent makes it 82 cols *)
           assert_pretty_print
             ~ctxt
             ("(\n  " ^ a80 ^ "\n)")
             (Group [Atom "("; Indent (Concat [line; Atom a80]); line; Atom ")"]) );
       ]
