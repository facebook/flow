// @flow
import { name as one } from "testproj";
import { name as two } from "testproj2";
import { name as three } from "testproj2/subfile";

import { name as four } from "testproj3";
import { name as five } from "testproj4";

import { name as six } from "testproj5";

import { name as seven } from "testproj6";

(one: "custom_resolve_dir/testproj");

(two: "subdir/custom_resolve_dir/testproj2");
(two: "custom_resolve_dir/testproj2"); // Error: Resolve from sibling 'custom_resolve_dir' first!
(two: "node_modules/testproj2"); // Error: Resolve from sibling 'custom_resolve_dir' first!

// if we fail to resolve in custom_resolve_dir
// then we should still resolve node_modules if
// possible.
//
// this tends to bring up the possibility of
// confusing bugs but it is consistent with
// the behavior of the webpack/babel features
// they are based upon.
//
// personally can see a flow lint warning
// as an option here if a conflict of folders
// is detected between aliases and/or node_modules
(three: "node_modules/testproj2/subfile");

// should still resolve node_modules
(four: "node_modules/testproj3");

// should still resolve custom_node_modules
(five: "custom_node_modules/testproj4");

(seven: "root_resolve_dir/testproj6/");
