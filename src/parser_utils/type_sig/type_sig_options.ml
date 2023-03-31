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
  module_ref_prefix: string option;
  module_ref_prefix_LEGACY_INTEROP: string option;
  enable_enums: bool;
  enable_relay_integration: bool;
  relay_integration_module_prefix: string option;
  conditional_type: bool;
  mapped_type: bool;
}

let of_parsing_options
    ?enable_relay_integration
    ?relay_integration_module_prefix
    ~munge
    ~ignore_static_propTypes
    ~facebook_keyMirror
    parsing_options =
  let open Parsing_options in
  let enable_relay_integration =
    Base.Option.value
      ~default:parsing_options.parse_enable_relay_integration
      enable_relay_integration
  in
  let relay_integration_module_prefix =
    Base.Option.value
      ~default:parsing_options.parse_relay_integration_module_prefix
      relay_integration_module_prefix
  in
  {
    munge;
    ignore_static_propTypes;
    facebook_keyMirror;
    enable_relay_integration;
    relay_integration_module_prefix;
    suppress_types = parsing_options.parse_suppress_types;
    facebook_fbt = parsing_options.parse_facebook_fbt;
    max_literal_len = parsing_options.parse_max_literal_len;
    exact_by_default = parsing_options.parse_exact_by_default;
    module_ref_prefix = parsing_options.parse_module_ref_prefix;
    module_ref_prefix_LEGACY_INTEROP = parsing_options.parse_module_ref_prefix_LEGACY_INTEROP;
    enable_enums = parsing_options.parse_enable_enums;
    conditional_type = parsing_options.parse_enable_conditional_types;
    mapped_type = parsing_options.parse_enable_mapped_types;
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
    module_ref_prefix = Options.haste_module_ref_prefix options;
    module_ref_prefix_LEGACY_INTEROP = Options.haste_module_ref_prefix_LEGACY_INTEROP options;
    enable_enums = Options.enums options;
    enable_relay_integration = Options.enable_relay_integration options;
    relay_integration_module_prefix = Options.relay_integration_module_prefix options;
    conditional_type = Options.conditional_type options;
    mapped_type = Options.mapped_type options;
  }
