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

// Substituted by the `paths` step for the test's sandbox project directory.
const PROJECT_ROOT_PLACEHOLDER = '<ROOT>';
const JSON_INDENT = 2;

// What a value actually passes through. `json` is sugar the caller writes; it expands to a parse
// where it appears plus a single stringify at the end, so the steps between it and the end operate
// on structured data rather than on text.
type Step = 'paths' | 'json_parse' | 'json_stringify';

function escapeRegExp(str: string): string {
  return str.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function expandTags(tags: $ReadOnlyArray<NormalizeTag>): Array<Step> {
  const steps: Array<Step> = [];
  let parses = false;
  for (const tag of tags) {
    const step: Step = match (tag) {
      'paths' => 'paths',
      'json' => 'json_parse',
    };
    steps.push(step);
    parses ||= step === 'json_parse';
  }
  if (parses) {
    steps.push('json_stringify');
  }
  return steps;
}

// Anchor the match so a sibling path sharing `projectDir` as a prefix (e.g. `/tmp/sandbox` vs
// `/tmp/sandbox2`) is not rewritten. Separators after the root are rewritten to `/` so one golden
// serves every platform; parse JSON first (the `json` tag) and this never sees an escaped path.
function normalizePathsInString(value: string, projectDir: string): string {
  const rooted = value.replace(
    new RegExp(`${escapeRegExp(projectDir)}(?![A-Za-z0-9_.-])`, 'g'),
    PROJECT_ROOT_PLACEHOLDER,
  );
  return rooted.replace(
    new RegExp(`${PROJECT_ROOT_PLACEHOLDER}\\S*`, 'g'),
    match => match.replace(/\\/g, '/'),
  );
}

function normalizePaths(value: mixed, projectDir: string): mixed {
  if (typeof value === 'string') {
    return normalizePathsInString(value, projectDir);
  }
  if (Array.isArray(value)) {
    return value.map(item => normalizePaths(item, projectDir));
  }
  if (typeof value === 'object' && value != null) {
    const out: {[string]: mixed} = {};
    for (const key of Object.keys(value)) {
      // Keys too: a manifest keyed by absolute path is otherwise unsnapshottable.
      out[normalizePathsInString(key, projectDir)] = normalizePaths(
        value[key],
        projectDir,
      );
    }
    return out;
  }
  return value;
}

function parseJson(value: mixed): mixed {
  if (typeof value !== 'string') {
    throw new Error("normalize: 'json' can only parse a string");
  }
  try {
    return JSON.parse(value);
  } catch (e) {
    throw new Error(`normalize: 'json' could not parse the value: ${e}`);
  }
}

function rootedPaths(value: mixed, projectDir: string): mixed {
  if (projectDir === '') {
    // A silent no-op here would let a golden be recorded still containing absolute sandbox paths —
    // the exact thing `paths` normalization exists to prevent. Fail loudly instead.
    throw new Error("normalize: 'paths' requested but no project dir is set");
  }
  return normalizePaths(value, projectDir);
}

function applyStep(step: Step, value: mixed, projectDir: string): mixed {
  return match (step) {
    'paths' => rootedPaths(value, projectDir),
    'json_parse' => parseJson(value),
    'json_stringify' => JSON.stringify(value, null, JSON_INDENT),
  };
}

// Apply the requested normalization as an ordered pipeline, and hand back text to snapshot against.
function normalizeOutput(
  tags: $ReadOnlyArray<NormalizeTag>,
  value: string,
  projectDir: string,
): string {
  let out: mixed = value;
  for (const step of expandTags(tags)) {
    out = applyStep(step, out, projectDir);
  }
  if (typeof out !== 'string') {
    throw new Error(
      "normalize: the pipeline produced a non-string; add 'json_stringify'",
    );
  }
  return out;
}

module.exports = {
  normalizeOutput,
  PROJECT_ROOT_PLACEHOLDER,
};
