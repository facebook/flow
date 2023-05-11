(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  suppress_types: SSet.t;
  munge: bool;
  ignore_static_propTypes: bool;
  facebook_keyMirror: bool;
  facebook_fbt: string option;
  max_literal_len: int;
  exact_by_default: bool;
  enable_enums: bool;
  enable_relay_integration: bool;
  relay_integration_module_prefix: string option;
  conditional_type: bool;
  mapped_type: bool;
  tuple_enhancements: bool;
  type_guards: bool;
}

let of_parsing_options parsing_options docblock file =
  let {
    Parsing_options.parse_munge_underscores = munge_underscores;
    parse_facebook_fbt = facebook_fbt;
    parse_suppress_types = suppress_types;
    parse_max_literal_len = max_literal_len;
    parse_exact_by_default = exact_by_default;
    parse_enable_enums = enable_enums;
    parse_enable_relay_integration = enable_relay_integration;
    parse_relay_integration_excludes = relay_integration_excludes;
    parse_relay_integration_module_prefix = relay_integration_module_prefix;
    parse_relay_integration_module_prefix_includes = relay_integration_module_prefix_includes;
    parse_enable_conditional_types = conditional_type;
    parse_enable_mapped_types = mapped_type;
    parse_tuple_enhancements = tuple_enhancements;
    parse_enable_type_guards = type_guards;
    _;
  } =
    parsing_options
  in
  let munge = munge_underscores && not (Docblock.preventMunge docblock) in
  (* NOTE: This is a temporary hack that makes the signature verifier ignore any static
     property named `propTypes` in any class. It should be killed with fire or replaced with
     something that only works for React classes, in which case we must make a corresponding
     change in the type system that enforces that any such property is private. *)
  let ignore_static_propTypes = true in
  (* NOTE: This is a Facebook-specific hack that makes the signature verifier and generator
     recognize and process a custom `keyMirror` function that makes an enum out of the keys
     of an object. *)
  let facebook_keyMirror = true in
  let enable_relay_integration =
    enable_relay_integration && Relay_options.enabled_for_file relay_integration_excludes file
  in
  let relay_integration_module_prefix =
    Relay_options.module_prefix_for_file
      relay_integration_module_prefix_includes
      file
      relay_integration_module_prefix
  in
  {
    munge;
    ignore_static_propTypes;
    facebook_keyMirror;
    enable_relay_integration;
    relay_integration_module_prefix;
    suppress_types;
    facebook_fbt;
    max_literal_len;
    exact_by_default;
    enable_enums;
    conditional_type;
    mapped_type;
    tuple_enhancements;
    type_guards;
  }

let of_options options ~munge ~ignore_static_propTypes ~facebook_keyMirror =
  {
    suppress_types = Options.suppress_types options;
    munge;
    ignore_static_propTypes;
    facebook_keyMirror;
    facebook_fbt = Options.facebook_fbt options;
    max_literal_len = Options.max_literal_length options;
    exact_by_default = Options.exact_by_default options;
    enable_enums = Options.enums options;
    enable_relay_integration = Options.enable_relay_integration options;
    relay_integration_module_prefix = Options.relay_integration_module_prefix options;
    conditional_type = Options.conditional_type options;
    mapped_type = Options.mapped_type options;
    tuple_enhancements = Options.tuple_enhancements options;
    type_guards = Options.type_guards options;
  }
