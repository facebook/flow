/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const {updateSuppressionsInText} = require('./update-suppressionsRunner');

const path = require('path');

const flowBinPath = path.resolve(process.env.FLOW_BIN);

test('updateSuppressionsInText removes unused comments', async () => {
  const testInput = `
// $FlowFixMe
const x = 4;
`.trimLeft();

  const loc = makeLoc(testInput, 1, 1, 1, 14);

  const testOutput = `
const x = 4;
`.trimLeft();

  const errorsByLine = addErrorByLine(new Map(), testInput, loc, [
    'foo',
    'bar',
  ]);
  await expectUpdatedComments(testInput, testOutput, errorsByLine, flowBinPath);
});

test('updateSuppressionsInText removes unused comments with sites', async () => {
  const testInput = `
// $FlowFixMe(site=bar,foo)
const x = 4;
`.trimLeft();

  const loc = makeLoc(testInput, 1, 1, 1, 28);

  const testOutput = `
const x = 4;
`.trimLeft();

  const errorsByLine = addErrorByLine(new Map(), testInput, loc, [
    'foo',
    'bar',
  ]);
  await expectUpdatedComments(testInput, testOutput, errorsByLine, flowBinPath);
});

test('updateSuppressionsInText can add site', async () => {
  const testInput = `
// $FlowFixMe - unused in foo
const x = 4;
`.trimLeft();

  const loc = makeLoc(testInput, 1, 1, 1, 30);

  const testOutput = `
// $FlowFixMe(site=bar) - unused in foo
const x = 4;
`.trimLeft();

  const errorsByLine = addErrorByLine(new Map(), testInput, loc, ['foo']);
  await expectUpdatedComments(testInput, testOutput, errorsByLine, flowBinPath);
});

test('updateSuppressionsInText keeps unknown sites', async () => {
  const testInput = `
// $FlowFixMe(site=other,foo)
const x = 4;
`.trimLeft();

  const loc = makeLoc(testInput, 1, 1, 1, 30);

  const testOutput = `
// $FlowFixMe(site=other)
const x = 4;
`.trimLeft();

  const errorsByLine = addErrorByLine(new Map(), testInput, loc, [
    'foo',
    'bar',
  ]);
  await expectUpdatedComments(testInput, testOutput, errorsByLine, flowBinPath);
});

test('updateSuppressionsInText keeps unknown sites (2)', async () => {
  const testInput = `
// $FlowFixMe(site=other)
const x = 4;
`.trimLeft();

  const loc = makeLoc(testInput, 1, 1, 1, 26);

  const testOutput = `
// $FlowFixMe(site=other)
const x = 4;
`.trimLeft();

  const errorsByLine = addErrorByLine(new Map(), testInput, loc, [
    'foo',
    'bar',
  ]);
  await expectUpdatedComments(testInput, testOutput, errorsByLine, flowBinPath);
});

test('updateSuppressionsInText adds site and keeps unknown sites', async () => {
  const testInput = `
// $FlowFixMe(site=other,foo)
const x = 4;
`.trimLeft();

  const loc = makeLoc(testInput, 1, 1, 1, 30);

  const testOutput = `
// $FlowFixMe(site=bar,other)
const x = 4;
`.trimLeft();

  const errorsByLine = addErrorByLine(new Map(), testInput, loc, ['foo']);
  await expectUpdatedComments(testInput, testOutput, errorsByLine, flowBinPath);
});

function addErrorByLine(errorsByLine, contents, loc, unusedRoots) {
  errorsByLine.set(loc.start.line, {
    unusedSuppressions: {
      roots: new Set(unusedRoots),
      bins: new Set().add(''),
      loc: loc,
    },
    errorCodes: new Set(),
    loc: loc,
  });
  return errorsByLine;
}

function makeLoc(contents, startLine, startCol, endLine, endCol) {
  return {
    start: {
      line: startLine,
      column: startCol,
      offset: posToOffset(contents, startLine, startCol),
    },
    end: {
      line: endLine,
      column: endCol,
      offset: posToOffset(contents, endLine, endCol),
    },
  };
}

function posToOffset(contents, line, col) {
  let offset = 0;
  // Using 1-indexed line and column for this
  let currentLine = 1;
  let currentCol = 1;
  const buf = Buffer.from(contents, 'utf8');
  while (offset < buf.length && !(currentLine === line && currentCol === col)) {
    const char = buf.toString('utf8', offset, offset + 1);
    if (char === '\n') {
      currentLine++;
      currentCol = 1;
    } else {
      currentCol++;
    }
    offset++;
  }

  return offset;
}

async function expectUpdatedComments(
  input,
  expectedOutput,
  errorsByLine,
  flowBinPath,
) {
  const actualOutput = await updateSuppressionsInText(
    Buffer.from(input),
    new Set(['foo', 'bar']),
    1,
    errorsByLine,
    '',
    flowBinPath,
  );
  expect(actualOutput.toString()).toEqual(expectedOutput);
}
