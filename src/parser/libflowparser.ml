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

let convert_options opts =
  let open Parser_env in
  if opts = [] then None else
  Some (List.fold_left (fun acc (k, v) ->
    match k with
    | "esproposal_class_instance_fields" -> { acc with esproposal_class_instance_fields = v }
    | "esproposal_class_static_fields" -> { acc with esproposal_class_static_fields = v }
    | "esproposal_decorators" -> { acc with esproposal_decorators = v }
    | "esproposal_export_star_as" -> { acc with esproposal_export_star_as = v }
    | "esproposal_optional_chaining" -> { acc with esproposal_optional_chaining = v }
    | "types" -> { acc with types = v }
    | "use_strict" -> { acc with use_strict = v }
    | _ -> acc (* ignore unknown stuff for future-compatibility *)
  ) Parser_env.default_parse_options opts)

let parse content options =
  let parse_options = convert_options options in
  let (ast, errors) = Parser_flow.program ~fail:false ~parse_options content in
  match Translate.program ast with
  | JObject params ->
    JObject (("errors", Translate.errors errors)::params)
  | _ -> assert false

(* TODO: register/handle Parse_error.Error *)
let () = Callback.register "flow_parse" parse
