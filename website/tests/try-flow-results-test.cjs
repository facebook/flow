/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @noflow
 * @format
 */

const assert = require('node:assert/strict');
const {test} = require('node:test');
const Module = require('node:module');
const path = require('node:path');
const {transformFileSync} = require('@babel/core');
const React = require('react');
const {renderToStaticMarkup} = require('react-dom/server');

function loadComponent(filename, exportName = 'default') {
  const compiled = new Module(filename, module);
  compiled.paths = Module._nodeModulePaths(path.dirname(filename));
  const originalRequire = compiled.require.bind(compiled);
  compiled.require = name => {
    if (name.endsWith('.css')) return {};
    if (name === 'react-json-view') return () => null;
    if (name === './clipboard') return () => Promise.resolve(true);
    if (name === './formatAstForDisplay') {
      return loadComponent(path.resolve(path.dirname(filename), `${name}.js`));
    }
    return originalRequire(name);
  };
  compiled._compile(
    transformFileSync(filename, {
      configFile: false,
      babelrc: false,
      presets: ['@babel/preset-flow', '@babel/preset-react'],
      plugins: [
        'babel-plugin-syntax-hermes-parser',
        '@babel/plugin-transform-modules-commonjs',
      ],
      parserOpts: {flow: 'all'},
    }).code,
    filename,
  );
  return compiled.exports[exportName];
}

const TryFlowResults = loadComponent(
  path.resolve(__dirname, '../src/try-flow/TryFlowResults.js'),
);

function render(overrides = {}) {
  return renderToStaticMarkup(
    React.createElement(TryFlowResults, {
      flowVersion: 'master',
      flowVersions: ['master', 'v0.300.0'],
      changeFlowVersion() {},
      loading: false,
      errors: [],
      internalError: '',
      cursorPosition: null,
      ast: {},
      onErrorSelect() {},
      ...overrides,
    }),
  );
}

// JSON is an error format, not a third top-level tab; Rich remains selected.
test('starts with Errors and AST tabs and Rich/JSON format pills', () => {
  const html = render();
  assert.match(html, /aria-pressed="true"[^>]*>.*Errors<\/button>/); // OK
  assert.match(html, /aria-pressed="false"[^>]*>.*AST<\/button>/); // OK
  assert.match(html, /role="group" aria-label="Error format"/); // OK
  assert.doesNotMatch(html, /<select aria-label="Error format"/); // OK
  assert.match(html, /aria-pressed="true"[^>]*>Rich<\/button>/); // OK
  assert.match(html, /aria-pressed="false"[^>]*>JSON<\/button>/); // OK
  assert.match(html, /No errors/); // OK
});

// The existing error cards, count, and editor-navigation targets remain intact.
test('renders error cards by default', () => {
  const html = render({
    errors: [
      {
        level: 'error',
        error_codes: ['incompatible-type'],
        message: [
          {
            loc: {
              source: '-',
              start: {line: 1, column: 1},
              end: {line: 1, column: 5},
            },
            context: 'value;',
            descr: 'Example diagnostic.',
          },
        ],
      },
    ],
  });
  assert.match(html, /Example diagnostic\./); // OK
  assert.match(html, /incompatible-type/); // OK
  assert.match(html, /aria-label="Go to error at line 1, column 1"/); // OK
  assert.match(html, /title="Copy all errors"/); // OK
  assert.match(html, /title="Copy this error"/); // OK
});

// Loading and internal errors must not look like a successful empty check.
test('distinguishes loading and internal failures from no errors', () => {
  assert.doesNotMatch(render({loading: true}), /No errors/); // OK
  const html = render({internalError: 'Example internal failure'});
  assert.match(html, /TryFlow encountered an internal error:/); // OK
  assert.match(html, /Example internal failure/); // OK
  assert.doesNotMatch(html, /No errors/); // OK
});

const AstResult = loadComponent(
  path.resolve(__dirname, '../src/try-flow/TryFlowResults.js'),
  'AstResult',
);

function renderAst(overrides = {}) {
  return renderToStaticMarkup(
    React.createElement(AstResult, {
      ast: {},
      format: 'tree',
      hideLocations: true,
      hideEmptyFields: true,
      cursorPosition: null,
      ...overrides,
    }),
  );
}

// JSON uses the same display filters as the tree, without changing the original AST.
test('serializes the filtered AST as indented JSON', () => {
  const loc = {source: 'example.js', start: {line: 1, column: 0}};
  const ast = {
    body: [{name: 'x', loc, optional: null, type: 'Identifier'}],
    loc,
    range: [0, 1],
    type: 'Program',
  };
  const original = JSON.stringify(ast);
  const expected = {type: 'Program', body: [{type: 'Identifier', name: 'x'}]};
  assert.equal(
    renderAst({ast, format: 'json'}),
    renderToStaticMarkup(
      React.createElement('pre', null, JSON.stringify(expected, null, 2)),
    ),
  ); // OK
  assert.equal(JSON.stringify(ast), original); // OK
  const shown = renderAst({
    ast,
    format: 'json',
    hideLocations: false,
    hideEmptyFields: false,
  });
  assert.match(shown, /&quot;loc&quot;:/); // OK
  assert.match(shown, /&quot;range&quot;:/); // OK
  assert.match(shown, /&quot;optional&quot;: null/); // OK
  assert.doesNotMatch(shown, /example\.js/); // OK
});

// Empty trees and parser/unsupported-version messages stay meaningful in either format.
test('preserves empty and string AST results in Tree and JSON modes', () => {
  for (const format of ['tree', 'json']) {
    assert.match(renderAst({format}), /No AST to show\./); // OK
    const ast = 'AST output is not supported in this version of Flow.';
    assert.equal(
      renderAst({ast, format}),
      renderToStaticMarkup(React.createElement('pre', null, ast)),
    ); // OK
  }
});
