(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let exports =
  if (Js.typeof (Js.Unsafe.js_expr "exports") != Js.string "undefined")
  then Js.Unsafe.js_expr "exports"
  else begin
    let exports = Js.Unsafe.obj [||] in
    Js.Unsafe.set Js.Unsafe.global "flow" exports;
    exports
  end
let () = Js.Unsafe.set exports "parse" (Js.wrap_callback Flow_parser_js.parse)
