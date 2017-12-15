(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* do NOT change the order of these constructors! the C code relies on ocaml's
   memory representation, which is order dependent. *)
type json =
  | JObject of (string * json) list
  | JArray of json list
  | JString of string
  | JNumber of float
  | JBool of bool
  | JNull

module AbstractTranslator : (
  Estree_translator.Translator with type t = json
) = struct
  type t = json
  let string x = JString x
  let bool x = JBool x
  let obj props = JObject props
  let array arr = JArray arr
  let number x = JNumber x
  let null = JNull
  let regexp _loc _pattern _flags = JNull
end

module Translate = Estree_translator.Translate (AbstractTranslator) (struct
  (* TODO: make these configurable via CLI flags *)
  let include_comments = true
  let include_locs = true
end)

(* TODO: parse_options *)
let parse content =
  let (ast, errors) = Parser_flow.program ~fail:false content in
  match Translate.program ast with
  | JObject params ->
    JObject (("errors", Translate.errors errors)::params)
  | _ -> assert false

(* TODO: register/handle Parse_error.Error *)
let () = Callback.register "flow_parse" parse
