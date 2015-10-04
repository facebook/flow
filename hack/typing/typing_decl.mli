(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
open Utils

val remove_classes: SSet.t -> unit

(*
 * This function works by side effects. It is adding in the
 * Naming_heap the nast produced from the ast passed as a parameter
 * (the SharedMem must thus have been initialized via SharedMem.init
 * prior to calling this function). It also assumes the Parser_heap
 * has been previously populated. It also adds dependencies
 * via Typing_deps.add_idep. It finally adds all the typing information
 * about classes, functions, typedefs, respectively in the globals
 * in Typing_env.Class, Typing_env.Fun, and Typing_env.Typedef.
 *)
val name_and_declare_types_program:
  Naming.env -> Relative_path.Set.t SMap.t (* set of classes in all files *) ->
  Ast.program -> unit

val make_env:
  Naming.env -> Relative_path.Set.t SMap.t ->
  Relative_path.t -> unit

val class_decl:
  TypecheckerOptions.t -> Nast.class_ -> unit
