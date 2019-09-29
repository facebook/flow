(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Js = Js_of_ocaml.Js

module JsTranslator : sig
  val translation_errors : (Loc.t * Parse_error.t) list ref

  include Translator_intf.S
end = struct
  type t = Js.Unsafe.any

  let translation_errors = ref []

  let string x = Js.Unsafe.inject (Js.string x)

  let bool x = Js.Unsafe.inject (Js.bool x)

  let obj props = Js.Unsafe.inject (Js.Unsafe.obj (Array.of_list props))

  let array arr = Js.Unsafe.inject (Js.array (Array.of_list arr))

  let number x = Js.Unsafe.inject (Js.number_of_float x)

  let int x = number (float x)

  let null = Js.Unsafe.inject Js.null

  let regexp loc pattern flags =
    let regexp =
      try Js.Unsafe.new_obj (Js.Unsafe.variable "RegExp") [|string pattern; string flags|]
      with _ ->
        translation_errors := (loc, Parse_error.InvalidRegExp) :: !translation_errors;

        (* Invalid RegExp. We already validated the flags, but we've been
         * too lazy to write a JS regexp parser in Ocaml, so we didn't know
         * the pattern was invalid. We'll recover with an empty pattern.
         *)
        Js.Unsafe.new_obj (Js.Unsafe.variable "RegExp") [|string ""; string flags|]
    in
    Js.Unsafe.inject regexp
end

module Translate =
  Estree_translator.Translate
    (JsTranslator)
    (struct
      let include_interned_comments = false

      let include_comments = true

      let include_locs = true
    end)

module Token_translator = Token_translator.Translate (JsTranslator)

let parse_options jsopts =
  Parser_env.(
    let opts = default_parse_options in
    let enums = Js.Unsafe.get jsopts "enums" in
    let opts =
      if Js.Optdef.test enums then
        { opts with enums = Js.to_bool enums }
      else
        opts
    in
    let decorators = Js.Unsafe.get jsopts "esproposal_decorators" in
    let opts =
      if Js.Optdef.test decorators then
        { opts with esproposal_decorators = Js.to_bool decorators }
      else
        opts
    in
    let class_instance_fields = Js.Unsafe.get jsopts "esproposal_class_instance_fields" in
    let opts =
      if Js.Optdef.test class_instance_fields then
        { opts with esproposal_class_instance_fields = Js.to_bool class_instance_fields }
      else
        opts
    in
    let class_static_fields = Js.Unsafe.get jsopts "esproposal_class_static_fields" in
    let opts =
      if Js.Optdef.test class_static_fields then
        { opts with esproposal_class_static_fields = Js.to_bool class_static_fields }
      else
        opts
    in
    let export_star_as = Js.Unsafe.get jsopts "esproposal_export_star_as" in
    let opts =
      if Js.Optdef.test export_star_as then
        { opts with esproposal_export_star_as = Js.to_bool export_star_as }
      else
        opts
    in
    let optional_chaining = Js.Unsafe.get jsopts "esproposal_optional_chaining" in
    let opts =
      if Js.Optdef.test optional_chaining then
        { opts with esproposal_optional_chaining = Js.to_bool optional_chaining }
      else
        opts
    in
    let nullish_coalescing = Js.Unsafe.get jsopts "esproposal_nullish_coalescing" in
    let opts =
      if Js.Optdef.test nullish_coalescing then
        { opts with esproposal_nullish_coalescing = Js.to_bool nullish_coalescing }
      else
        opts
    in
    let types = Js.Unsafe.get jsopts "types" in
    let opts =
      if Js.Optdef.test types then
        { opts with types = Js.to_bool types }
      else
        opts
    in
    opts)

let translate_tokens offset_table tokens =
  JsTranslator.array (List.rev_map (Token_translator.token offset_table) tokens)

let parse content options =
  let options =
    if options = Js.undefined then
      Js.Unsafe.obj [||]
    else
      options
  in
  let content = Js.to_string content in
  let parse_options = Some (parse_options options) in
  let include_tokens =
    let tokens = Js.Unsafe.get options "tokens" in
    Js.Optdef.test tokens && Js.to_bool tokens
  in
  let rev_tokens = ref [] in
  let token_sink =
    if include_tokens then
      Some (fun token_data -> rev_tokens := token_data :: !rev_tokens)
    else
      None
  in
  let (ocaml_ast, errors) = Parser_flow.program ~fail:false ~parse_options ~token_sink content in
  JsTranslator.translation_errors := [];
  let offset_table = Offset_utils.make content in
  let ret = Translate.program (Some offset_table) ocaml_ast in
  let translation_errors = !JsTranslator.translation_errors in
  Js.Unsafe.set ret "errors" (Translate.errors (errors @ translation_errors));
  if include_tokens then Js.Unsafe.set ret "tokens" (translate_tokens offset_table !rev_tokens);
  ret
