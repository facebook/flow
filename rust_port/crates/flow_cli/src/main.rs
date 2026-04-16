/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[allow(non_upper_case_globals)]
#[unsafe(no_mangle)]
#[used]
static malloc_conf: &str = "metadata_thp:always\0";

fn main() {
    flow_cli_support::main();
}
