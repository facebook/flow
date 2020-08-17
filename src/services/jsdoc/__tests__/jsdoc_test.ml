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

let string_option_printer = function
  | None -> "None"
  | Some desc -> Printf.sprintf "Some %S" desc

let mk_test comment ?(should_not_parse = false) ?description ?params ctxt =
  match Jsdoc.of_comments comment with
  | None -> assert_bool "JSDoc didn't parse" should_not_parse
  | Some jsdoc ->
    Base.Option.iter description ~f:(fun description ->
        assert_equal
          ~ctxt
          ~printer:string_option_printer
          ~msg:"description"
          description
          (Jsdoc.description jsdoc));
    Base.Option.iter
      params
      ~f:
        (Base.List.iter ~f:(fun (name, description) ->
             assert_equal
               ~ctxt
               ~printer:string_option_printer
               ~msg:(Printf.sprintf "param %s" name)
               description
               (Jsdoc.param jsdoc name)))

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
         "simple_param_descriptions"
         >:: mk_test
               (mk_block_comment
                  {|* overall description
                    * @param a description for a
                    * @param {ignore this} b description for b
                    * @param c - hyphen before description for c
                    * @param d
                    * @param e  multiline
                    *           param description
                    * @unsupported tag to be skipped
                    * @arg f - arg alias for param tag
                    * @argument g - argument alias for param tag
                    * @param h description before eof|})
               ~description:(Some "overall description")
               ~params:
                 [
                   ("a", Some "description for a");
                   ("b", Some "description for b");
                   ("c", Some "hyphen before description for c");
                   ("d", None);
                   ("e", Some "multiline\nparam description");
                   ("f", Some "arg alias for param tag");
                   ("g", Some "argument alias for param tag");
                   ("h", Some "description before eof");
                 ];
       ]
