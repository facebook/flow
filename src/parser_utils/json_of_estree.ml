(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Hh_json_translator : Translator_intf.S with type t = Hh_json.json = struct
  type t = Hh_json.json

  open Hh_json

  let string x = JSON_String x

  let bool x = JSON_Bool x

  let obj props = JSON_Object props

  let array arr = JSON_Array arr

  let number x = JSON_Number (Dtoa.ecma_string_of_float x)

  let int x = JSON_Number (string_of_int x)

  let null = JSON_Null

  let regexp _loc _pattern _flags = JSON_Null
end

include Hh_json_translator
