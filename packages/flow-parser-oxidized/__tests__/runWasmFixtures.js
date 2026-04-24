/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

'use strict';

const {test} = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const util = require('node:util');

// Bypasses the public `../src` (i.e. `index.js`) entry on purpose. `parse()`
// in index.js applies hermes-parser-compatible adapter fixups (literalType
// synthesis, ChainExpression wrapping, docblock attachment, etc.) so that
// downstream JS tooling sees a canonical hermes-parser-shape AST. The
// fixtures under flow/src/parser/test/flow/ were captured against the OCaml
// Flow parser's raw output, which doesn't have those fixups, so we go
// straight to the underlying deserializer here. Don't "fix" this back to
// `require('../src')` — that breaks ~12 fixtures with adapter-shape diffs.
// Drop-in / contract parity with hermes-parser is exercised separately by
// the jest tests under this directory; that suite goes through index.js.
const FlowParser = require('../src/FlowParser');

// Walk directory tree returning sorted list of files relative to root.
function listFiles(root, dir) {
  const files = fs.readdirSync(dir ? path.join(root, dir) : root);
  const result = [];
  for (const f of files) {
    const rel = dir ? path.join(dir, f) : f;
    const stats = fs.statSync(path.join(root, rel));
    if (stats.isDirectory()) {
      result.push(...listFiles(root, rel));
    } else {
      result.push(rel);
    }
  }
  return result.sort();
}

// Group files into per-section, per-case structures matching the OCaml/JS
// hardcoded test runner discovery.
function getTests(rootDir) {
  const files = listFiles(rootDir);
  const tests = {};
  for (const file of files) {
    const sectionName = path.dirname(file);
    const caseParts = path.basename(file).split('.');
    const caseName = caseParts[0];

    // Hidden files
    if (caseName === '') {
      continue;
    }

    if (!tests[sectionName]) {
      tests[sectionName] = {};
    }
    if (!tests[sectionName][caseName]) {
      tests[sectionName][caseName] = {};
    }
    const c = tests[sectionName][caseName];

    const content = fs.readFileSync(path.join(rootDir, file), {
      encoding: 'utf8',
    });
    const ext = caseParts[caseParts.length - 1];
    const kind = caseParts.length > 2 ? caseParts[caseParts.length - 2] : null;

    if (ext === 'js') {
      c.content = content;
    } else if (ext === 'flow') {
      c.content = content;
      c.filename = path.join(rootDir, file);
    } else if (ext === 'json' && kind === 'tree') {
      c.expected_ast = JSON.parse(content);
    } else if (ext === 'ts' || ext === 'mts' || ext === 'cts') {
      c.content = content;
      c.filename = path.join(rootDir, file);
    } else if (ext === 'json' && kind === 'options') {
      c.options = JSON.parse(content);
    }
  }
  return tests;
}

// Comparison ported verbatim from
// flow/packages/flow-parser/test/hardcoded_test_runner.js so the wasm tests
// apply the same diffing rules the existing OCaml-backed flow_parser tests
// use.
function newEnv() {
  const diffs = {};
  const path = ['root'];
  return {
    push_path: path.push.bind(path),
    pop_path: path.pop.bind(path),
    diff(type, expected, actual) {
      const pathStr = path.join('.');
      diffs[pathStr] = {
        path: pathStr,
        type,
        expected,
        actual,
      };
    },
    get_diffs() {
      const ret = [];
      for (const prop in diffs) {
        if (Object.prototype.hasOwnProperty.call(diffs, prop)) {
          ret.push(diffs[prop]);
        }
      }
      return ret;
    },
  };
}

function diffToString(diff) {
  let expectedStr = '';
  if (typeof diff.expected !== 'undefined') {
    expectedStr =
      '. Expected ' +
      JSON.stringify(diff.expected) +
      ', got ' +
      JSON.stringify(diff.actual);
  }
  return diff.path + ': ' + diff.type + expectedStr;
}

function compare(env, ast, spec, skipComments) {
  if (Array.isArray(spec)) {
    if (Array.isArray(ast)) {
      if (spec.length !== ast.length) {
        env.diff('Array has wrong size', spec.length, ast.length);
      }
      for (let i = 0; i < spec.length; i++) {
        if (Object.prototype.hasOwnProperty.call(spec, i)) {
          if (Object.prototype.hasOwnProperty.call(ast, i)) {
            env.push_path(i);
            compare(env, ast[i], spec[i], skipComments);
            env.pop_path();
          }
        }
      }
    } else {
      env.diff('Not an array');
    }
  } else if (spec != null && typeof spec === 'object') {
    for (const prop in spec) {
      const isCommentsProp =
        prop === 'trailingComments' || prop === 'leadingComments';
      if (skipComments && isCommentsProp) {
        continue;
      }
      // Skip comparing loc.source when expected is null (filename comparison).
      // Matches the OCaml test runner behavior where include_filename = false.
      if (prop === 'source' && spec[prop] === null) {
        continue;
      }
      if (Object.prototype.hasOwnProperty.call(spec, prop)) {
        const segments = prop.split('.');
        let subAst = ast;
        let found = true;
        let i;
        for (i = 0; i < segments.length; i++) {
          const seg = segments[i];
          if (subAst && Object.prototype.hasOwnProperty.call(subAst, seg)) {
            subAst = subAst[seg];
          } else {
            env.diff('Missing property "' + seg + '"');
            found = false;
            break;
          }
          env.push_path(seg);
        }
        if (found) {
          compare(env, subAst, spec[prop], skipComments);
        }
        for (; i > 0; i--) {
          env.pop_path();
        }
      }
    }
  } else {
    if (ast !== spec && !(ast instanceof RegExp && spec === null)) {
      env.diff('Wrong value', spec, ast);
    }
  }
}

function escapeContent(content) {
  return content
    .replace(/[\\]/g, '\\\\')
    .replace(/[\b]/g, '\\b')
    .replace(/[\f]/g, '\\f')
    .replace(/[\n]/g, '\\n')
    .replace(/[\r]/g, '\\r')
    .replace(/[\t]/g, '\\t');
}

function runOneCase(testCase, parseOptions) {
  // Discovery may emit cases that lack one half of the (.js, .tree.json) pair.
  // The OCaml/JS runners simply skip those — they have no ground truth to
  // compare against. We surface them as a hard failure so a missing fixture is
  // never silent.
  if (testCase.content == null) {
    return {
      passed: false,
      output: 'Missing source content (no .js / .flow / .ts file)',
    };
  }
  if (testCase.expected_ast == null) {
    return {
      passed: false,
      output: 'Missing expected AST (no .tree.json file)',
    };
  }

  const result = {passed: true, output: ''};
  const append = (...args) => {
    result.output += args.join(' ') + '\n';
  };

  let flowAst;
  try {
    flowAst = FlowParser.parse(testCase.content, {
      ...parseOptions,
      sourceFilename: testCase.filename,
    });
  } catch (e) {
    append('Flow exploded:', util.inspect(e, {depth: null}));
    result.passed = false;
    return result;
  }

  const env = newEnv();
  const flowErrors = flowAst.errors;
  // The wasm parser's ESTree output may not surface an `errors` array (Flow's
  // ESTree shape collects parse errors out-of-band when configured). Treat
  // missing as empty so the existing comparison logic still applies.
  const errors = Array.isArray(flowErrors) ? flowErrors : [];

  compare(env, flowAst, testCase.expected_ast, true);

  const diffs = env.get_diffs();
  if (diffs.length > 0) {
    result.passed = false;
    append('****Unexpected Differences****');
    for (let i = 0; i < diffs.length; i++) {
      append('(#' + i + ')', diffToString(diffs[i]));
    }
  }

  let expectedToError =
    Object.prototype.hasOwnProperty.call(testCase.expected_ast, 'errors') &&
    testCase.expected_ast.errors.length !== 0;
  for (const prop in testCase.expected_ast) {
    if (
      Object.prototype.hasOwnProperty.call(testCase.expected_ast, prop) &&
      prop.match(/^errors\./)
    ) {
      expectedToError = true;
      break;
    }
  }

  if (errors.length !== 0 && !expectedToError) {
    result.passed = false;
    append('****Unexpected Errors****');
    for (let i = 0; i < errors.length; i++) {
      const e = errors[i];
      append(
        '(#' + i + ')',
        '(line ' +
          e.loc.start.line +
          ', col ' +
          e.loc.start.column +
          ') - ' +
          '(line ' +
          e.loc.end.line +
          ', col ' +
          e.loc.end.column +
          '): ',
        e.message,
      );
    }
  }

  return result;
}

// Discovery + registration. The fixture root is taken from
// FLOW_PARSER_FIXTURE_ROOT so the BUCK runtime can point at the materialized
// runtime_files directory; falls back to the in-tree path for local runs.
const FIXTURE_ROOT =
  process.env.FLOW_PARSER_FIXTURE_ROOT ||
  path.resolve(__dirname, '../../../src/parser/test/flow');

const sections = getTests(FIXTURE_ROOT);
const sectionNames = Object.keys(sections).sort();

if (sectionNames.length === 0) {
  throw new Error(
    'No fixture sections discovered under ' +
      FIXTURE_ROOT +
      '. Set FLOW_PARSER_FIXTURE_ROOT to point at flow/src/parser/test/flow.',
  );
}

// Translate OCaml-style fixture options (as written in `*.options.json` files
// — `components`, `enums`, `pattern_matching`, `types`, etc.) into the
// JS-layer `flow.parse` options (`enableExperimentalComponentSyntax`,
// `enableExperimentalFlowMatchSyntax`, `flow`). The OCaml runner consumes the
// fixture options directly via `Parser_env.parse_options`; the JS layer
// renamed them, so we bridge here so the same fixture files work.
function translateFixtureOptions(fixtureOptions) {
  const out = {};
  for (const k of Object.keys(fixtureOptions)) {
    const v = fixtureOptions[k];
    switch (k) {
      case 'components':
        out.enableExperimentalComponentSyntax = v;
        break;
      case 'pattern_matching':
        out.enableExperimentalFlowMatchSyntax = v;
        break;
      case 'types':
        // OCaml's `default_parse_options` sets `types = true`; fixtures that
        // exercise the no-type-grammar path (e.g. ts_syntax `as`/`satisfies`
        // disabled) opt out via `types: false`. Plumbed through the WASM C
        // ABI as `enableTypes`.
        out.enableTypes = v;
        break;
      case 'esproposal_decorators':
        // The Rust parser exposes a `enableExperimentalDecorators` flag
        // through the WASM ABI so fixtures that disable decorators (e.g.
        // `decorators/class_field_disabled`) can exercise the
        // unexpected-`@` error path.
        out.enableExperimentalDecorators = v;
        break;
      case 'assert_operator':
        // Flow's `expr!` non-null assertion. Off by default; fixtures opt in
        // via `assert_operator: true`. Plumbed through the WASM C ABI into
        // the parser's `assert_operator` flag.
        out.assertOperator = v;
        break;
      case 'enums':
        // OCaml's `default_parse_options` sets `enums = false`; fixtures
        // that exercise enum syntax opt in via `enums: true` in their
        // `.options.json`. Plumbed through the WASM C ABI as `enableEnums`.
        out.enableEnums = v;
        break;
      case 'records':
        // OCaml's `default_parse_options` sets `records = false`; fixtures
        // that exercise record syntax (`#{ ... }`) opt in via
        // `records: true`. Plumbed through the WASM C ABI as `enableRecords`.
        out.enableRecords = v;
        break;
      case 'use_strict':
      case 'intern_comments':
        // No matching JS-layer option; the wasm parser hardwires safe
        // defaults that match the fixture expectations.
        break;
      default:
        // Pass through unknown keys (sourceFilename, tokens, etc.).
        out[k] = v;
        break;
    }
  }
  return out;
}

for (const sectionName of sectionNames) {
  const cases = sections[sectionName];
  const caseNames = Object.keys(cases).sort();
  test(sectionName, async t => {
    for (const caseName of caseNames) {
      await t.test(caseName, () => {
        const c = cases[caseName];
        const sectionOptions = {};
        const caseOptions = translateFixtureOptions(c.options || {});
        const parseOptions = {...sectionOptions, ...caseOptions};
        const r = runOneCase(c, parseOptions);
        if (!r.passed) {
          assert.fail(
            'Fixture ' +
              sectionName +
              '/' +
              caseName +
              ' (source: "' +
              escapeContent(c.content || '<missing>') +
              '")\n' +
              r.output,
          );
        }
      });
    }
  });
}
