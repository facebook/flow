(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module M = Map_ast
open Ast

(*
  Desugars constructor argument promotion by hack.
  Example:
    old: public class Foo {
            public function __construct(private $foo) {}
         }
    new: public class Foo {
            private $foo;
            public function __construct($foo) {
              $this->foo = $foo;
            }
         }
 *)

(*
  The Hack Parser automatically promotes constructor arguments into fields and
   initializes them in the constructor. (arguably wrongly so). However, as long
   as this is the case, the only thing the tool needs to do is to
   drop the modifier on the param.
*)
let strip_access_modifier (k, _) fun_param =
  k { fun_param with param_modifier = None;}

let map =
  M.mk_program_mapper { M.default_mapper with
    M.k_fun_param = strip_access_modifier;
  }
