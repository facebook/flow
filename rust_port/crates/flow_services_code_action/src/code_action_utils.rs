/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_common::options::Options;
use flow_parser_utils_output::js_layout_generator::Opts;
use flow_parser_utils_output::js_layout_generator::TrailingCommas;

pub fn layout_options(options: &Options) -> Opts {
    Opts {
        bracket_spacing: options.format.bracket_spacing,
        preserve_formatting: false,
        single_quotes: options.format.single_quotes,
        trailing_commas: TrailingCommas::All,
    }
}
