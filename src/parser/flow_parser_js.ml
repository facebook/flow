(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module JsTranslator : sig
  val translation_errors: (Loc.t * Parse_error.t) list ref
  include Estree_translator.Translator
end = struct
  type t = Js.Unsafe.any

  let translation_errors = ref []

  let string x = Js.Unsafe.inject (Js.string x)
  let bool x = Js.Unsafe.inject (Js.bool x)
  let obj props = Js.Unsafe.inject (Js.Unsafe.obj props)
  let array arr = Js.Unsafe.inject (Js.array arr)
  let number x = Js.Unsafe.inject (Js.number_of_float x)
  let null = Js.Unsafe.inject Js.null
  let regexp loc pattern flags =
    let regexp = try
      Js.Unsafe.new_obj (Js.Unsafe.variable "RegExp") [|
        string pattern;
        string flags;
      |]
    with _ ->
      translation_errors := (loc, Parse_error.InvalidRegExp)::!translation_errors;
      (* Invalid RegExp. We already validated the flags, but we've been
       * too lazy to write a JS regexp parser in Ocaml, so we didn't know
       * the pattern was invalid. We'll recover with an empty pattern.
       *)
      Js.Unsafe.new_obj (Js.Unsafe.variable "RegExp") [|
        string "";
        string flags;
      |]
    in
    Js.Unsafe.inject regexp
end

let throw e =
  let fn = (Js.Unsafe.new_obj (Js.Unsafe.variable "Function") [|
    Js.Unsafe.inject (Js.string "e");
    Js.Unsafe.inject (Js.string "throw e;");
  |]) in
  Js.Unsafe.call fn fn [| e|]

let parse_options jsopts = Parser_env.(
  let opts = default_parse_options in

  let decorators = Js.Unsafe.get jsopts "esproposal_decorators" in
  let opts = if Js.Optdef.test decorators
    then { opts with esproposal_decorators = Js.to_bool decorators; }
    else opts in

  let class_instance_fields = Js.Unsafe.get jsopts "esproposal_class_instance_fields" in
  let opts = if Js.Optdef.test class_instance_fields
    then { opts with esproposal_class_instance_fields = Js.to_bool class_instance_fields; }
    else opts in

  let class_static_fields = Js.Unsafe.get jsopts "esproposal_class_static_fields" in
  let opts = if Js.Optdef.test class_static_fields
    then { opts with esproposal_class_static_fields = Js.to_bool class_static_fields; }
    else opts in

  let export_star_as = Js.Unsafe.get jsopts "esproposal_export_star_as" in
  let opts = if Js.Optdef.test export_star_as
    then { opts with esproposal_export_star_as = Js.to_bool export_star_as; }
    else opts in

  let types = Js.Unsafe.get jsopts "types" in
  let opts = if Js.Optdef.test types
    then { opts with types = Js.to_bool types; }
    else opts in

  opts
)

let parse content options =
  let content = Js.to_string content in
  let parse_options = Some (parse_options options) in
  try
    let (ocaml_ast, errors) = Parser_flow.program ~fail:false ~parse_options content in
    JsTranslator.translation_errors := [];
    let module Translate = Estree_translator.Translate (JsTranslator) in
    let ret = Translate.program ocaml_ast in
    let translation_errors = !JsTranslator.translation_errors in
    Js.Unsafe.set ret "errors" (Translate.errors (errors @ translation_errors));
    ret
  with Parse_error.Error l ->
    let e = Js.Unsafe.new_obj (Js.Unsafe.variable "Error") [|
      Js.Unsafe.inject (Js.string ((string_of_int (List.length l))^" errors"));
    |] in
    Js.Unsafe.set e "name" ((Js.string "Parse Error"));
    ignore (throw e);
    Js.Unsafe.obj [||]
