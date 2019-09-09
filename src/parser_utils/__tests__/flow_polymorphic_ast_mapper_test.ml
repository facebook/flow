(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

class ['a] mapper =
  object
    inherit [Loc.t, 'a, Loc.t, 'a] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot (x : Loc.t) = x

    method on_type_annot (x : 'a) = x
  end

(* these tests don't do much other than check that the mapper doesn't raise
    exceptions *)

let run_mapper source =
  let (ast, _) = Parser_flow.program source in
  let mapper = new mapper in
  let _ = mapper#program ast in
  ()

let tests =
  "polymorphic ast mapper"
  >::: [ ( "simple"
         >:: fun _ ->
         let source = "function foo() { (5 * 3); 4; (6 + 4); }" in
         run_mapper source ) ]
