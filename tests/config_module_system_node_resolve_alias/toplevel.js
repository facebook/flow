// @flow

import { name as one } from "testproj";
import { name as two } from "testproj2";
import { name as incrementalNodeModules } from "incrementalNodeModules";

(one: "custom_resolve_dir/testproj");
(one: "node_modules/testproj"); // Error: Resolve from resolve_alias first!

// at base level we resolve local testproj2
// as opposed to the local custom_resolve_dir
// in the subdir test
(two: "custom_resolve_dir/testproj2");

(incrementalNodeModules: "node_modules/incrementalNodeModules");
