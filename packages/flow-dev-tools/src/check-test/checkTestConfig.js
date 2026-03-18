/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const fs = require('fs');
const {join} = require('path');

export type TestConfig = {
  auto_start: boolean,
  shell: string,
  cmd: string,
  stdin: string,
  ignore_stderr: boolean,
  cwd: string,
  file_watcher: string,
  start_args: string,
  wait_for_recheck: string,
  skip_saved_state: boolean,
  saved_state_only: boolean,
  git: boolean,
  skip_rust_port: boolean,
};

async function parseTestConfig(testDir: string): Promise<TestConfig> {
  const config: TestConfig = {
    auto_start: true,
    shell: '',
    cmd: 'full-check',
    stdin: '',
    ignore_stderr: true,
    cwd: '',
    file_watcher: 'none',
    start_args: '',
    wait_for_recheck: 'true',
    skip_saved_state: false,
    saved_state_only: false,
    git: false,
    skip_rust_port: false,
  };

  const configPath = join(testDir, '.testconfig');
  try {
    await fs.promises.access(configPath);
  } catch {
    return config;
  }

  const content = await fs.promises.readFile(configPath, 'utf8');
  const lines = content.split('\n');

  // Track whether cmd was explicitly set in the config file.
  // In bash, shell is processed first (clears cmd), then cmd is processed
  // (overrides). This means cmd always wins regardless of file order.
  // We match that by deferring the shell→cmd clearing to after the loop.
  let cmdExplicitlySet = false;

  for (const line of lines) {
    const trimmed = line.trim();
    if (trimmed === '' || trimmed.startsWith('#')) continue;

    const colonIndex = trimmed.indexOf(':');
    if (colonIndex === -1) continue;

    const key = trimmed.substring(0, colonIndex).trim();
    const value = trimmed.substring(colonIndex + 1).trim();

    switch (key) {
      case 'auto_start':
        config.auto_start = value !== 'false';
        break;
      case 'shell':
        config.shell = value;
        break;
      case 'cmd': {
        // cmd takes the rest of the line after "cmd:" with whitespace trimmed.
        // Use .trim() (not .replace(/^ /, '')) to handle any amount of
        // whitespace after the colon, matching bash's awk field-splitting
        // which normalizes leading whitespace.
        config.cmd = trimmed.substring(colonIndex + 1).trim();
        cmdExplicitlySet = true;
        break;
      }
      case 'stdin':
        config.stdin = value;
        break;
      case 'ignore_stderr':
        config.ignore_stderr = value !== 'false';
        break;
      case 'cwd':
        config.cwd = value;
        break;
      case 'file_watcher':
        if (value !== '') {
          config.file_watcher = value;
        }
        break;
      case 'start_args': {
        // start_args takes everything after "start_args:" with whitespace
        // trimmed. Use .trim() to match bash's awk field-splitting which
        // normalizes leading whitespace. Without this, extra spaces after
        // the colon leave residual whitespace that causes split(/\s+/) to
        // produce empty array elements.
        config.start_args = trimmed.substring(colonIndex + 1).trim();
        break;
      }
      case 'wait_for_recheck':
        config.wait_for_recheck = value === 'false' ? 'false' : 'true';
        break;
      case 'skip_saved_state':
        config.skip_saved_state = value === 'true';
        break;
      case 'saved_state_only':
        config.saved_state_only = value === 'true';
        break;
      case 'git':
        config.git = value === 'true';
        break;
      case 'skip_rust_port':
        config.skip_rust_port = value === 'true';
        break;
    }
  }

  // Match bash semantics: if shell is set but cmd was NOT explicitly
  // overridden, clear cmd. If cmd was explicitly set, it wins.
  if (config.shell !== '' && !cmdExplicitlySet) {
    config.cmd = '';
  }

  return config;
}

module.exports = {parseTestConfig};
