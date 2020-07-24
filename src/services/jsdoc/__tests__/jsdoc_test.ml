(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

(************************)
(* AST construction helpers *)
(************************)

let mk_line_comment text =
  let open Flow_ast in
  Some
    Syntax.
      {
        leading = [((), Comment.{ text; kind = Line; on_newline = false })];
        trailing = [];
        internal = ();
      }

let mk_block_comment text =
  let open Flow_ast in
  Some
    Syntax.
      {
        leading = [((), Comment.{ text; kind = Block; on_newline = false })];
        trailing = [];
        internal = ();
      }

let mk_block_comments ~leading ~trailing =
  let open Flow_ast in
  let block text = ((), Comment.{ text; kind = Block; on_newline = false }) in
  Some
    Syntax.{ leading = List.map block leading; trailing = List.map block trailing; internal = () }

(***********)
(* testing *)
(***********)

let mk_test comment ?(should_not_parse = false) ?description ctxt =
  match Jsdoc.of_comments comment with
  | None -> assert_bool "JSDoc didn't parse" should_not_parse
  | Some jsdoc ->
    Base.Option.iter description ~f:(fun description ->
        assert_equal
          ~ctxt
          ~printer:(function
            | None -> "None"
            | Some desc -> Printf.sprintf "Some %S" desc)
          ~msg:"description"
          description
          (Jsdoc.description jsdoc))

let tests =
  "JSDoc"
  >::: [
         "dont_parse_line" >:: mk_test (mk_line_comment "* foo") ~should_not_parse:true;
         "parse_description" >:: mk_test (mk_block_comment "* foo") ~description:(Some "foo");
         "trim_whitespace_and_asterisks"
         >:: mk_test
               (mk_block_comment
                  {|*
                    *   foo
                    *   bar
                    *   @snap crackle
                    *   |})
               ~description:(Some "foo\nbar");
         "pick_last_jsdoc-containing_leading_comment"
         >:: mk_test
               (mk_block_comments
                  ~leading:["* foo"; "bar"; "* baz"; "snap"]
                  ~trailing:["* crackle"])
               ~description:(Some "baz");
         "no_description" >:: mk_test (mk_block_comment "*\n @unsupported") ~description:None;
       ]
