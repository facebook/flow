/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const flowParserJsPath = process.argv[2];
const flowParser = require(flowParserJsPath);

// Test 1: Parse without filename - source should be null
const resultWithoutFilename = flowParser.parse('const x = 1;');
if (resultWithoutFilename.loc.source !== null) {
  throw new Error(
    'Expected source to be null when no filename provided, got: ' +
      JSON.stringify(resultWithoutFilename.loc.source),
  );
}

// Test 2: Parse with filename - source should match the provided filename
const testFilename = 'test/my-file.js';
const resultWithFilename = flowParser.parse('const x = 1;', {
  filename: testFilename,
});
if (resultWithFilename.loc.source !== testFilename) {
  throw new Error(
    'Expected source to be "' +
      testFilename +
      '", got: ' +
      JSON.stringify(resultWithFilename.loc.source),
  );
}

// Test 3: Verify filename is also in nested node locations
const body = resultWithFilename.body;
if (body.length !== 1) {
  throw new Error('Expected 1 statement in body, got: ' + body.length);
}
const varDecl = body[0];
if (varDecl.loc.source !== testFilename) {
  throw new Error(
    'Expected nested node source to be "' +
      testFilename +
      '", got: ' +
      JSON.stringify(varDecl.loc.source),
  );
}

// Test 4: Verify tokens work with filename option (tokens use different loc format without source)
const resultWithTokens = flowParser.parse('const x = 1;', {
  filename: testFilename,
  tokens: true,
});
if (!resultWithTokens.tokens || resultWithTokens.tokens.length === 0) {
  throw new Error('Expected tokens to be present');
}
// Tokens have their own loc format (start/end without source), just verify tokens parse correctly
const firstToken = resultWithTokens.tokens[0];
if (firstToken.type !== 'T_CONST') {
  throw new Error(
    'Expected first token to be T_CONST, got: ' +
      JSON.stringify(firstToken.type),
  );
}

// Test 5: Verify .js.flow files can parse Flow type annotations
const flowFileResult = flowParser.parse('const foo: string;', {
  filename: 'test/types.js.flow',
});
if (flowFileResult.errors.length > 0) {
  throw new Error(
    'Expected no errors when parsing type annotations in .js.flow file, got: ' +
      JSON.stringify(flowFileResult.errors),
  );
}
// Verify the source is set correctly
if (flowFileResult.loc.source !== 'test/types.js.flow') {
  throw new Error(
    'Expected source to be "test/types.js.flow", got: ' +
      JSON.stringify(flowFileResult.loc.source),
  );
}
// Verify the type annotation was parsed
const fooDecl = flowFileResult.body[0];
if (!fooDecl.declarations || !fooDecl.declarations[0].id.typeAnnotation) {
  throw new Error('Expected type annotation to be parsed in .js.flow file');
}

console.log('All flow_parser.js filename option tests passed!');
