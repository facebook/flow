/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import type {NormalizeTag} from './assertionTypes';

// Substituted by the `paths` tag for the test's sandbox project directory.
const PROJECT_ROOT_PLACEHOLDER = '<ROOT>';

function escapeRegExp(str: string): string {
  return str.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

// Apply the requested normalization tags, in order, to `value`. An unknown tag is a hard failure
// rather than a silent no-op, keeping the tag set closed.
function normalizeOutput(
  tags: $ReadOnlyArray<NormalizeTag>,
  value: string,
  projectDir: string,
): string {
  let out = value;
  for (const tag of tags) {
    switch (tag) {
      case 'paths':
        if (projectDir === '') {
          // A silent no-op here would let a golden be recorded still containing absolute sandbox
          // paths — the exact thing `paths` normalization exists to prevent. Fail loudly instead.
          throw new Error(
            "normalize: ['paths'] requested but no project dir is set",
          );
        }
        // Replace the sandbox project directory with a stable placeholder. Anchor the match so a
        // sibling path sharing `projectDir` as a prefix (e.g. `/tmp/sandbox` vs `/tmp/sandbox2`)
        // is not rewritten: the directory must be followed by a path separator or a token boundary.
        out = out.replace(
          new RegExp(`${escapeRegExp(projectDir)}(?![A-Za-z0-9_.-])`, 'g'),
          PROJECT_ROOT_PLACEHOLDER,
        );
        break;
      default:
        throw new Error(
          `unknown normalize tag: ${String(tag)} (known tags: paths)`,
        );
    }
  }
  return out;
}

module.exports = {
  normalizeOutput,
  PROJECT_ROOT_PLACEHOLDER,
};
