import {name} from "testproj2";

name as "node_modules/testproj2"; // Error: Resolve from sibling 'custom_resolve_dir' first!
name as "subdir/custom_resolve_dir/testproj2";
