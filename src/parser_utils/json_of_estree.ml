(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Hh_json_translator : (
  Estree_translator.Translator with type t = Hh_json.json
) = struct
  type t = Hh_json.json

  open Hh_json

  let string x = JSON_String x
  let bool x = JSON_Bool x
  let obj props = JSON_Object (Array.to_list props)
  let array arr = JSON_Array (Array.to_list arr)
  let number x = JSON_Number (Utils_js.string_of_float_trunc x)
  let null = JSON_Null
  let regexp _loc _pattern _flags = JSON_Null
end

include Hh_json_translator
