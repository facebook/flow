(**
 *  Copyright 2012-2014 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
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
