/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {execSync, spawnSync} = require('child_process');
const fs = require('fs');
const path = require('path');

function getReleases() /*: Array<string> */ {
  // GitHub API Docs: https://docs.github.com/en/rest/releases/releases
  const authHeader = process.env.FLOW_BOT_TOKEN
    ? `-H "Authorization: Bearer ${process.env.FLOW_BOT_TOKEN}"`
    : '';
  const options =
    spawnSync('type', ['fwdproxy-config'], {shell: true, stdio: 'pipe'})
      .status == 0
      ? '$(fwdproxy-config curl) '
      : '';
  const cmd = `curl ${authHeader} ${options}"https://api.github.com/repos/facebook/flow/releases?per_page=80" | jq --raw-output '.[] | .tag_name'`;
  return execSync(cmd, {stdio: ['ignore', 'pipe', 'ignore']})
    .toString('utf8')
    .trim()
    .split('\n');
}

const CACHE_FILE = '.docusaurus/flow-versions-cache.json';

function getCachedReleases() /*: Array<string> */ {
  if (!process.env.INCLUDE_PAST_RELEASES) return [];
  if (fs.existsSync(CACHE_FILE)) {
    return JSON.parse(fs.readFileSync(CACHE_FILE).toString());
  } else {
    const releases = getReleases();
    fs.mkdirSync(path.dirname(CACHE_FILE), {recursive: true});
    fs.writeFileSync(CACHE_FILE, JSON.stringify(releases));
    return releases;
  }
}

const allFlowVersions /*: Array<string> */ = ['master', ...getCachedReleases()];

module.exports = allFlowVersions;
