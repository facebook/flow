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

let string_of_option (f : 'a -> string) : 'a option -> string = function
  | None -> "None"
  | Some x -> Printf.sprintf "Some %s" (f x)

let mk_test comment ?(should_not_parse = false) ?description ?params ctxt =
  match Jsdoc.of_comments comment with
  | None -> assert_bool "JSDoc didn't parse" should_not_parse
  | Some jsdoc ->
    Base.Option.iter description ~f:(fun description ->
        assert_equal
          ~ctxt
          ~cmp:(Base.Option.equal String.equal)
          ~printer:(string_of_option (Printf.sprintf "%S"))
          ~msg:"description"
          description
          (Jsdoc.description jsdoc));
    Base.Option.iter params ~f:(fun params ->
        assert_equal
          ~ctxt
          ~cmp:Jsdoc.Params.equal
          ~printer:Jsdoc.Params.show
          ~msg:"params"
          params
          (Jsdoc.params jsdoc))

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
                      @param
                      @param e  multiline
                    *           param description
                    * @unsupported tag to be skipped
                    * @arg f - arg alias for param tag
                    * @argument g - argument alias for param tag
                      @param
                      @param h
                      @param i description before eof|})
               ~description:(Some "overall description")
               ~params:
                 Jsdoc.Param.
                   [
                     ( "a",
                       [(Name, { description = Some "description for a"; optional = NotOptional })]
                     );
                     ( "b",
                       [(Name, { description = Some "description for b"; optional = NotOptional })]
                     );
                     ( "c",
                       [
                         ( Name,
                           {
                             description = Some "hyphen before description for c";
                             optional = NotOptional;
                           } );
                       ] );
                     ("d", [(Name, { description = None; optional = NotOptional })]);
                     ( "e",
                       [
                         ( Name,
                           {
                             description = Some "multiline\nparam description";
                             optional = NotOptional;
                           } );
                       ] );
                     ( "f",
                       [
                         ( Name,
                           { description = Some "arg alias for param tag"; optional = NotOptional }
                         );
                       ] );
                     ( "g",
                       [
                         ( Name,
                           {
                             description = Some "argument alias for param tag";
                             optional = NotOptional;
                           } );
                       ] );
                     ("h", [(Name, { description = None; optional = NotOptional })]);
                     ( "i",
                       [
                         ( Name,
                           { description = Some "description before eof"; optional = NotOptional }
                         );
                       ] );
                   ];
         "advanced_params"
         >:: mk_test
               (mk_block_comment
                  {|*
                    * @param [foo] - optional foo
                    * @param bar[] - element path of bar
                    * @param bar.x - member x path of bar
                    * @param [bar.y=this is a default] - optional member y path of bar with default
                    * @param [baz[]=this is another default] - optional element path of baz with default
                    * @param baz.bar.foo member foo of member bar of baz
                    * @param baz[].foo.bar member bar of member foo of element of baz
                    * @param qux.x.y[].z
                    * @param [qux[][].x]
                    * @param [qux=has a default]
                  |})
               ~params:
                 Jsdoc.Param.
                   [
                     ("foo", [(Name, { description = Some "optional foo"; optional = Optional })]);
                     ( "bar",
                       [
                         ( Element Name,
                           { description = Some "element path of bar"; optional = NotOptional } );
                         ( Member (Name, "x"),
                           { description = Some "member x path of bar"; optional = NotOptional } );
                         ( Member (Name, "y"),
                           {
                             description = Some "optional member y path of bar with default";
                             optional = OptionalWithDefault "this is a default";
                           } );
                       ] );
                     ( "baz",
                       [
                         ( Element Name,
                           {
                             description = Some "optional element path of baz with default";
                             optional = OptionalWithDefault "this is another default";
                           } );
                         ( Member (Member (Name, "bar"), "foo"),
                           {
                             description = Some "member foo of member bar of baz";
                             optional = NotOptional;
                           } );
                         ( Member (Member (Element Name, "foo"), "bar"),
                           {
                             description = Some "member bar of member foo of element of baz";
                             optional = NotOptional;
                           } );
                       ] );
                     ( "qux",
                       [
                         ( Member (Element (Member (Member (Name, "x"), "y")), "z"),
                           { description = None; optional = NotOptional } );
                         ( Member (Element (Element Name), "x"),
                           { description = None; optional = Optional } );
                         ( Name,
                           { description = None; optional = OptionalWithDefault "has a default" } );
                       ] );
                   ];
       ]
