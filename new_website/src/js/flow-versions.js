/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {execSync} = require('child_process');

function gitTagVersions() {
  const cmd = `git ls-remote --tags 2>/dev/null`;
  const versionStrings = execSync(cmd, {cwd: '..', shell: true})
    .toString('utf8')
    .trim()
    .split('\n')
    .map(line => {
      let versionString = line;
      // Remove version hash
      versionString = versionString.split('\t')[1];
      // refs/tags/v0.100.0 => v0.100.0
      versionString = versionString.substring('refs/tags/'.length);
      // v0.100.0-rc => v0.100.0
      versionString = versionString.split('-')[0];
      // v0.100.0^{} => v0.100.0
      versionString = versionString.split('^')[0];
      // v0.100.0 => 0.100.0
      if (versionString.startsWith('v')) {
        versionString = versionString.substring(1);
      }
      return versionString;
    });
  return Array.from(new Set(versionStrings), versionString =>
    versionString.split('.').map(s => parseInt(s, 10)),
  )
    .sort(([major1, minor1, patch1], [major2, minor2, patch2]) => {
      let c = major2 - major1;
      if (c != 0) return c;
      c = minor2 - minor1;
      if (c != 0) return c;
      return patch2 - patch1;
    })
    .map(([major, minor, patch]) => `v${major}.${minor}.${patch}`);
}

function getFlowVersions(
  excludeReleases /*: ?boolean */ = false,
  excludeMaster /*: ?boolean */ = false,
) /*: Array<string> */ {
  const versions = [];
  if (!excludeMaster) {
    versions.push('master');
  }
  if (!excludeReleases) {
    versions.push(...gitTagVersions());
  }
  return versions;
}

module.exports = getFlowVersions;
