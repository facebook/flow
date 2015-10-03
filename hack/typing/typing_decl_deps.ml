(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Module adding the dependencies related to inheritance.
 * We need a complete accurate graph of dependencies related to inheritance.
 * Because without them, we can't recompute the set of files that must be
 * rechecked when something changes.
 *)

module Dep = Typing_deps.Dep

open Ast
open Core

let rec add defl = List.iter defl def

and def = function
  | Class c -> class_ c
  | Fun _  | Stmt _  | Typedef _ | Constant _ -> ()
  | Namespace _ | NamespaceUse _ -> assert false

and class_ c =
  let name = snd c.c_name in
  List.iter c.c_extends (hint name);
  List.iter c.c_implements (hint name);
  List.iter c.c_body (class_def name)

and class_def root = function
  | ClassUse h -> hint root h
  | XhpAttrUse h -> hint root h
  | ClassTraitRequire (_, h) -> hint root h
  | Attributes _  | Const _ | AbsConst _ | ClassVars _ | XhpAttr _ | Method _
  | TypeConst _ | XhpCategory _ -> ()

and hint root (_, h) =
  match h with
  | Happly ((_, parent), _) ->
      Typing_deps.add_idep (Some (Dep.Class root)) (Dep.Extends parent)
  | Hoption _ | Hfun _ | Htuple _ | Hshape _ | Haccess _ -> ()
