(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(**
 * Quick and dirty Json pretty printing library.
 *
 *)

type json =
    JList of json list
  | JBool of bool
  | JString of string
  | JAssoc of (string * json) list
  | JNull
  | JInt of int


let rec to_js_object json =
  match json with
  | JList l ->
      let l = List.map to_js_object l in
      let l = Array.of_list l in
      Js.Unsafe.inject (Js.array l)
  | JAssoc l ->
      let l = List.map begin fun (k, v) ->
        k, to_js_object v
      end l in
      let l = Array.of_list l in
      Js.Unsafe.obj l
  | JBool b -> Js.Unsafe.inject (Js.bool b)
  | JString s -> Js.Unsafe.inject (Js.string s)
  | JNull -> Js.Unsafe.inject Js.null
  | JInt i -> Js.Unsafe.inject (Js.number_of_float (float_of_int i))
