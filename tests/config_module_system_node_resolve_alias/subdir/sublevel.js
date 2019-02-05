// @flow
import { name as one } from "testproj";
import { name as two } from "testproj2";
import { name as three } from "testproj3";
import { name as four } from "testproj4";

(one: "custom_resolve_dir/testproj");

(two: "subdir/custom_resolve_dir/testproj2");
(two: "custom_resolve_dir/testproj2"); // Error: Resolve from sibling 'custom_resolve_dir' first!
(two: "node_modules/testproj2"); // Error: Resolve from sibling 'custom_resolve_dir' first!

// should still resolve node_modules
(three: "node_modules/testproj3");

// should still resolve custom_node_modules
(four: "custom_node_modules/testproj4");
