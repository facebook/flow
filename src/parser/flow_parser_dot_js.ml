(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Js = Js_of_ocaml.Js

let () =
  let exports =
    if Js.typeof (Js.Unsafe.js_expr "exports") != Js.string "undefined" then
      Js.Unsafe.js_expr "exports"
    else
      let exports = Js.Unsafe.obj [||] in
      Js.Unsafe.set Js.Unsafe.global "flow" exports;
      exports
  in
  let js_error_of_exn = function
    | Js.Error e -> Js.raise_js_error e
    | exn ->
      let msg = "Internal error: " ^ Printexc.to_string exn in
      Js.raise_js_error (Js.Unsafe.new_obj Js.error_constr [| Js.Unsafe.inject (Js.string msg) |])
  in
  let parse content options =
    (try Flow_parser_js.parse content options with exn -> js_error_of_exn exn)
  in
  Js.Unsafe.set exports "parse" (Js.Unsafe.callback parse)
