(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  suppress_types: SSet.t;
  munge: bool;
  facebook_keyMirror: bool;
  facebook_fbt: string option;
  max_literal_len: int;
  exact_by_default: bool;
  enable_declare_global: bool;
  enable_enums: bool;
  enable_component_syntax: bool;
  component_syntax_enabled_in_config: bool;
  enable_ts_syntax: bool;
  hook_compatibility: bool;
  casting_syntax: Options.CastingSyntax.t;
  enable_relay_integration: bool;
  relay_integration_module_prefix: string option;
  for_builtins: bool;
  locs_to_dirtify: Loc.t list;
}

let of_options options docblock locs_to_dirtify file =
  let munge = Options.should_munge_underscores options && not (Docblock.preventMunge docblock) in
  (* NOTE: This is a Facebook-specific hack that makes the signature verifier and generator
     recognize and process a custom `keyMirror` function that makes an enum out of the keys
     of an object. *)
  let facebook_keyMirror = true in
  let enable_relay_integration = Options.enable_relay_integration options in
  let relay_integration_excludes = Options.relay_integration_excludes options in
  let relay_integration_module_prefix = Options.relay_integration_module_prefix options in
  let relay_integration_module_prefix_includes =
    Options.relay_integration_module_prefix_includes options
  in
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
    facebook_keyMirror;
    enable_relay_integration;
    relay_integration_module_prefix;
    locs_to_dirtify;
    hook_compatibility = Options.hook_compatibility_in_file options file;
    suppress_types = Options.suppress_types options;
    facebook_fbt = Options.facebook_fbt options;
    max_literal_len = Options.max_literal_length options;
    exact_by_default = Options.exact_by_default options;
    enable_declare_global = Options.enable_declare_global options;
    enable_enums = Options.enums options;
    enable_component_syntax = Options.typecheck_component_syntax_in_file options file;
    component_syntax_enabled_in_config = Options.component_syntax options;
    enable_ts_syntax = Options.ts_syntax options;
    for_builtins = false;
    casting_syntax = Options.casting_syntax options;
  }

let builtin_options options =
  (* NOTE: This is a Facebook-specific hack that makes the signature verifier and generator
     recognize and process a custom `keyMirror` function that makes an enum out of the keys
     of an object. *)
  let facebook_keyMirror = true in
  {
    munge = false;
    facebook_keyMirror;
    enable_relay_integration = false;
    relay_integration_module_prefix = None;
    suppress_types = Options.suppress_types options;
    facebook_fbt = Options.facebook_fbt options;
    max_literal_len = Options.max_literal_length options;
    exact_by_default = Options.exact_by_default options;
    enable_enums = Options.enums options;
    enable_component_syntax = true;
    enable_declare_global = Options.enable_declare_global options;
    component_syntax_enabled_in_config = Options.component_syntax options;
    enable_ts_syntax = false;
    hook_compatibility = Options.hook_compatibility options;
    casting_syntax = Options.casting_syntax options;
    for_builtins = true;
    locs_to_dirtify = [];
  }
