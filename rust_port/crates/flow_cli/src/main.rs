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
    #[cfg(fbcode_build)]
    {
        flow_cli_support::register_extra_commands(|| {
            vec![flow_facebook_rage::rage_command::command()]
        });
        flow_cli_support::register_docs_url(
            "https://www.internalfb.com/intern/staticdocs/flow/en/docs/",
        );
    }
    flow_cli_support::main();
}
