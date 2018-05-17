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
  Translator_intf.S with type t = json
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

module Token_translator = Token_translator.Translate (AbstractTranslator)

let translate_tokens tokens =
  AbstractTranslator.array (List.rev_map Token_translator.token tokens)

let convert_options opts =
  let open Parser_env in
  List.fold_left (fun (opts, tokens) (k, v) ->
    match k with
    | "esproposal_class_instance_fields" ->
        { opts with esproposal_class_instance_fields = v }, tokens
    | "esproposal_class_static_fields" ->
        { opts with esproposal_class_static_fields = v }, tokens
    | "esproposal_decorators" ->
        { opts with esproposal_decorators = v }, tokens
    | "esproposal_export_star_as" ->
        { opts with esproposal_export_star_as = v }, tokens
    | "esproposal_optional_chaining" ->
        { opts with esproposal_optional_chaining = v }, tokens
    | "types" ->
        { opts with types = v }, tokens
    | "use_strict" ->
        { opts with use_strict = v }, tokens
    | "tokens" ->
        opts, v
    | _ ->
        opts, tokens (* ignore unknown stuff for future-compatibility *)
  ) (Parser_env.default_parse_options, false) opts

let parse content options =
  let parse_options, include_tokens = convert_options options in

  let rev_tokens = ref [] in
  let token_sink =
    if include_tokens then Some (fun token_data -> rev_tokens := token_data::!rev_tokens)
    else None
  in

  let (ast, errors) = Parser_flow.program
    ~fail:false ~parse_options:(Some parse_options) ~token_sink
    content
  in

  match Translate.program ast with
  | JObject params ->
    let params = ("errors", Translate.errors errors)::params in
    let params =
      if include_tokens then ("tokens", translate_tokens !rev_tokens)::params
      else params
    in
    JObject params
  | _ -> assert false

(* TODO: register/handle Parse_error.Error *)
let () = Callback.register "flow_parse" parse
