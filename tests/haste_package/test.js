// @flow

// this package has haste_commonjs: true
import haste_pkg from 'haste_pkg'; // ok

// Haste shouldn't allow this (it requires haste_commonjs), but Metro
// doesn't implement haste_commonjs. Metro treats any package not in
// node_modules as a Haste package.
import non_haste_pkg from 'non_haste_pkg'; // ok

// this is in a node_modules folder that is not in the parent hierarchy,
// so we should not be able to see it from this file.
import node_pkg from 'node_pkg'; // error

// this is in a node_modules folder that is not in the parent hierarchy,
// but it has haste_commonjs: true, so we allow it.
import haste_node_pkg from 'haste_node_pkg'; // ok
