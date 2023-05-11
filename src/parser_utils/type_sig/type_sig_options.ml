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

let of_options options docblock file =
  let munge = Options.should_munge_underscores options && not (Docblock.preventMunge docblock) in
  (* NOTE: This is a temporary hack that makes the signature verifier ignore any static
     property named `propTypes` in any class. It should be killed with fire or replaced with
     something that only works for React classes, in which case we must make a corresponding
     change in the type system that enforces that any such property is private. *)
  let ignore_static_propTypes = true in
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
    ignore_static_propTypes;
    facebook_keyMirror;
    enable_relay_integration;
    relay_integration_module_prefix;
    suppress_types = Options.suppress_types options;
    facebook_fbt = Options.facebook_fbt options;
    max_literal_len = Options.max_literal_length options;
    exact_by_default = Options.exact_by_default options;
    enable_enums = Options.enums options;
    conditional_type = Options.conditional_type options;
    mapped_type = Options.mapped_type options;
    tuple_enhancements = Options.tuple_enhancements options;
    type_guards = Options.type_guards options;
  }

let builtin_options options =
  (* NOTE: This is a temporary hack that makes the signature verifier ignore any static
     property named `propTypes` in any class. It should be killed with fire or replaced with
     something that only works for React classes, in which case we must make a corresponding
     change in the type system that enforces that any such property is private. *)
  let ignore_static_propTypes = true in
  (* NOTE: This is a Facebook-specific hack that makes the signature verifier and generator
     recognize and process a custom `keyMirror` function that makes an enum out of the keys
     of an object. *)
  let facebook_keyMirror = true in
  {
    munge = false;
    ignore_static_propTypes;
    facebook_keyMirror;
    enable_relay_integration = false;
    relay_integration_module_prefix = None;
    suppress_types = Options.suppress_types options;
    facebook_fbt = Options.facebook_fbt options;
    max_literal_len = Options.max_literal_length options;
    exact_by_default = Options.exact_by_default options;
    enable_enums = Options.enums options;
    conditional_type = Options.conditional_type options;
    mapped_type = Options.mapped_type options;
    tuple_enhancements = Options.tuple_enhancements options;
    type_guards = Options.type_guards options;
  }
