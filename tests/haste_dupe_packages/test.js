// @flow

/**
 * This tests that we detect duplicate haste packages
 */

import pkg from "pkg";

// should choose a/pkg as the provider, which exports a number
(pkg: empty);
