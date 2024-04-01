(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let layout_options options =
  let open Js_layout_generator in
  {
    default_opts with
    bracket_spacing = Options.format_bracket_spacing options;
    single_quotes = Options.format_single_quotes options;
  }
