// @flow

import {name} from "testproj2";

(name: "subdir/custom_resolve_dir/testproj2");
(name: "custom_resolve_dir/testproj2"); // Error: Resolve from sibling 'custom_resolve_dir' first!
(name: "node_modules/testproj2"); // Error: Resolve from sibling 'custom_resolve_dir' first!
