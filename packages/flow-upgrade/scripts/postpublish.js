/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

/*
 * This file will not be transformed by Babel so we don't use Flow or modern
 * JavaScript syntax not supported by Node.js.
 */

'use strict';

const path = require('path');
const fs = require('fs');
const cp = require('child_process');

const pkgPath = path.resolve(__dirname, '../package.json');
const pkg = JSON.parse(fs.readFileSync(pkgPath, 'utf8'));

// If the package name is `flow-upgrade` then we want to rename it to
// `create-flow-upgrade` and publish. If the name is `create-flow-upgrade` then
// we want to rename back to `flow-upgrade`.
if (pkg.name === 'flow-upgrade') {
  pkg.name = 'create-flow-upgrade';
  fs.writeFileSync(pkgPath, JSON.stringify(pkg, null, 2));
  cp.spawnSync(process.env.npm_execpath, ['publish'], {
    cwd: path.resolve(__dirname, '..'),
    env: process.env,
    stdio: 'inherit',
  });
} else if (pkg.name === 'create-flow-upgrade') {
  pkg.name = 'flow-upgrade';
  fs.writeFileSync(pkgPath, JSON.stringify(pkg, null, 2));
}
