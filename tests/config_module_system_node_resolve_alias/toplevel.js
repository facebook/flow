// @flow

import {name} from "testproj";

(name: "custom_resolve_dir/testproj"); 
(name: "node_modules/testproj"); // Error: Resolve from resolve_alias first!