(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*
module Spider_Monkey_Ast_Js : Ast.SPIDER_MONKEY_AST =
  struct
    (*
    type node = int
    type position = <line: Js.number Js.t; column: Js.number Js.t> Js.t

    let number_of_int x = Js.number_of_float (float_of_int x)

    let pos line column : position =
      assert (line >= 1);
      assert (column >= 0);
      let p = Js.Unsafe.obj [||] in
      p##line <- number_of_int line;
      p##column <- number_of_int column;
      p
      *)
    type prog = int
    type stmt = int
    let program l = 123
    let empty_statement = 123;
  end
*)
