(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let () =
  let exports =
    if (Js.typeof (Js.Unsafe.js_expr "exports") != Js.string "undefined")
    then Js.Unsafe.js_expr "exports"
    else begin
      let exports = Js.Unsafe.obj [||] in
      Js.Unsafe.set Js.Unsafe.global "flow" exports;
      exports
    end
  in
  let js_error_of_exn = function
    | Js.Error e ->
        Js.raise_js_error e
    | exn ->
        let msg = "Internal error: "^(Printexc.to_string exn) in
        Js.raise_js_error (Js.Unsafe.new_obj Js.error_constr [|
          Js.Unsafe.inject (Js.string msg)
        |])
  in
  let parse content options =
    try Flow_parser_js.parse content options
    with exn -> js_error_of_exn exn
  in
  Js.Unsafe.set exports "parse" (Js.Unsafe.callback parse)
