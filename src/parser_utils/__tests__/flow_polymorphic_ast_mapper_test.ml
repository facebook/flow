(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

(* these tests don't do much other than check that the mapper doesn't raise
    exceptions *)

let run_mapper source =
  let ast, _ = Parser_flow.program source in
  let mapper = new Flow_polymorphic_ast_mapper.mapper (fun _ -> ()) (fun _ -> ()) in
  let _ = mapper#program ast in
  ()

let tests = "polymorphic ast mapper" >::: [
  "simple" >:: (fun _ ->
    let source = "function foo() { (5 * 3); 4; (6 + 4); }" in
    run_mapper source;
)]
