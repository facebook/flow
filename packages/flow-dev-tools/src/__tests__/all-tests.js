/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

require('flow-remove-types/register');

const path = require('path');

const {exec} = require('../utils/async');
const {splitIntoChunks} = require('../utils/string');
const {
  updateSuppressionsInText,
} = require('../update-suppressions/update-suppressionsRunner');
const {
  removeUnusedErrorSuppressionsFromText,
} = require('../comment/remove-commentsRunner');
const {addCommentsToCode} = require('../comment/add-commentsRunner');

class Expected {
  constructor(actualValue) {
    this.actualValue = actualValue;
  }

  toBe(expectedValue) {
    if (this.actualValue !== expectedValue) {
      throw new Error(`Expected ${this.actualValue} to be ${expectedValue}`);
    }
  }

  toEqual(expectedValue) {
    const actual = JSON.stringify(this.actualValue);
    const expected = JSON.stringify(expectedValue);
    if (actual !== expected) {
      throw new Error(`Expected ${actual} to be ${expected}`);
    }
  }
}

function expect(v) {
  return new Expected(v);
}

function repeatString(str, times) {
  let result = '';
  for (let i = 0; i < times; i++) {
    result += str;
  }
  return result;
}

const collectedTests = [];

function test(name, fn) {
  collectedTests.push({name, fn});
}

test('updateSuppressionsInText does not remove eslint-disable comment', async () => {
  const testInput = `
// $FlowFixMe eslint-disable-next-line no-fallthrough
const x = 4;
`.trimLeft();

  const loc = makeLoc(testInput, 1, 1, 1, 54);

  const testOutput = `
// $FlowFixMe eslint-disable-next-line no-fallthrough
const x = 4;
`.trimLeft();

  const errorsByLine = addErrorByLine(
    new Map(),
    testInput,
    loc,
    [],
    ['foo', 'bar'],
  );
  await expectUpdatedComments(testInput, testOutput, errorsByLine, flowBinPath);
});

test('updateSuppressionsInText does not remove eslint-disable multiline comment', async () => {
  const testInput = `
/* $FlowFixMe
 * eslint-disable no-fallthrough
 */
const x = 4;
`.trimLeft();

  const loc = makeLoc(testInput, 1, 1, 3, 4);

  const testOutput = `
/* $FlowFixMe
 * eslint-disable no-fallthrough
 */
const x = 4;
`.trimLeft();

  const errorsByLine = addErrorByLine(
    new Map(),
    testInput,
    loc,
    [],
    ['foo', 'bar'],
  );
  await expectUpdatedComments(testInput, testOutput, errorsByLine, flowBinPath);
});

test('exec', async () => {
  expect(await exec('echo foo')).toBe('foo\n');
  expect(await exec('cat', {stdin: 'bar'})).toBe('bar');

  expect(repeatString('foo', 2)).toBe('foofoo');

  // make the string big enough that it exceeds the chunk size for writes
  const repeatedString = repeatString('0123456789', 2000);
  expect(await exec('cat', {stdin: repeatedString})).toBe(repeatedString);
});

test('splitIntoChunks', () => {
  expect(splitIntoChunks('', 1)).toEqual([]);
  expect(splitIntoChunks('abcd', 2)).toEqual(['ab', 'cd']);
  expect(splitIntoChunks('abc', 2)).toEqual(['ab', 'c']);
  expect(splitIntoChunks('abc', 10)).toEqual(['abc']);
  expect(splitIntoChunks('abc', 1)).toEqual(['a', 'b', 'c']);
  // The check marks are multi-byte characters when encoded with UTF-8. Make sure they are treated
  // as single characters and not split up into individual bytes.
  expect(splitIntoChunks('✓✓✓✓✓', 1)).toEqual(['✓', '✓', '✓', '✓', '✓']);
});

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

  const errorsByLine = addErrorByLine(
    new Map(),
    testInput,
    loc,
    [],
    ['foo', 'bar'],
  );
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

  const errorsByLine = addErrorByLine(
    new Map(),
    testInput,
    loc,
    [],
    ['foo', 'bar'],
  );
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

  const errorsByLine = addErrorByLine(new Map(), testInput, loc, [], ['foo']);
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

  const errorsByLine = addErrorByLine(
    new Map(),
    testInput,
    loc,
    [],
    ['foo', 'bar'],
  );
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

  const errorsByLine = addErrorByLine(
    new Map(),
    testInput,
    loc,
    [],
    ['foo', 'bar'],
  );
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

  const errorsByLine = addErrorByLine(new Map(), testInput, loc, [], ['foo']);
  await expectUpdatedComments(testInput, testOutput, errorsByLine, flowBinPath);
});

test('updateSuppressionsInText handles JSX children', async () => {
  const testInput = `
(<Foo>
  {someExpr}
  {anotherExpr}
</Foo>);
`.trimLeft();

  const loc = makeLoc(testInput, 1, 7, 4, 1);

  const testOutput = `
// $FlowFixMe[code]
(<Foo>
  {someExpr}
  {anotherExpr}
</Foo>);
`.trimLeft();

  const errorsByLine = addErrorByLine(new Map(), testInput, loc, ['code'], []);
  await expectUpdatedComments(
    testInput,
    testOutput,
    errorsByLine,
    flowBinPath,
    [],
  );
});

test('updateSuppressionsInText handles JSX children with multiline open tag', async () => {
  const testInput = `
(<Foo
  bar="baz">
  {someExpr}
  {anotherExpr}
</Foo>);
`.trimLeft();

  const loc = makeLoc(testInput, 2, 13, 5, 1);

  const testOutput = `
(<Foo
  // $FlowFixMe[code]
  bar="baz">
  {someExpr}
  {anotherExpr}
</Foo>);
`.trimLeft();

  const errorsByLine = addErrorByLine(new Map(), testInput, loc, ['code'], []);
  await expectUpdatedComments(
    testInput,
    testOutput,
    errorsByLine,
    flowBinPath,
    [],
  );
});

function addErrorByLine(errorsByLine, contents, loc, errorCodes, unusedRoots) {
  const unusedSuppressions =
    unusedRoots && unusedRoots.length > 0
      ? {
          roots: new Set(unusedRoots),
          bins: new Set().add(''),
          loc: loc,
        }
      : undefined;
  errorsByLine.set(loc.start.line, {
    unusedSuppressions,
    errorCodes: new Set(errorCodes),
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
  sites = ['foo', 'bar'],
) {
  const actualOutput = await updateSuppressionsInText(
    Buffer.from(input),
    new Set(sites),
    1,
    errorsByLine,
    '',
    flowBinPath,
  );
  expect(actualOutput.toString()).toEqual(expectedOutput);
}

test('addCommentsToCode > basic', async () => {
  expect(await addCommentsToCode('foobar', null, '', [], flowBinPath)).toEqual([
    '',
    0,
  ]);
});

test('addCommentsToCode > advanced', async () => {
  expect(
    await addCommentsToCode(
      longComment,
      null,
      testInput,
      /* Intentionally made these out of order to test that they are still inserted properly */
      [1, 6, 5, 3].map(line => makeSuppression(line, testInput)),
      flowBinPath,
    ),
  ).toEqual([testOutput, 8]);
});

test('addCommentsToCode > empty function params', async () => {
  expect(
    await addCommentsToCode(
      '',
      null,
      `function foo() {}
function bar<a>(): b {}
(function <a>(): b {});
(<a>(): b => {});
`,
      [
        {
          loc: {
            start: {line: 1, column: 13, offset: 13},
            end: {line: 1, column: 14, offset: 14},
          },
          isError: true,
          lints: new Set(),
          error_codes: ['code1'],
        },
        {
          loc: {
            start: {line: 2, column: 16, offset: 34},
            end: {line: 2, column: 17, offset: 35},
          },
          isError: true,
          lints: new Set(),
          error_codes: ['code2'],
        },
        {
          loc: {
            start: {line: 3, column: 14, offset: 56},
            end: {line: 3, column: 15, offset: 57},
          },
          isError: true,
          lints: new Set(),
          error_codes: ['code3'],
        },
        {
          loc: {
            start: {line: 4, column: 5, offset: 71},
            end: {line: 4, column: 6, offset: 72},
          },
          isError: true,
          lints: new Set(),
          error_codes: ['code4'],
        },
      ],
      flowBinPath,
    ),
  ).toEqual([
    `// $FlowFixMe[code1]
function foo() {}
// $FlowFixMe[code2]
function bar<a>(): b {}
// $FlowFixMe[code3]
(function <a>(): b {});
// $FlowFixMe[code4]
(<a>(): b => {});
`,
    4,
  ]);
});

test('addCommentsToCode > does not touch AST when no errors match the given code', async () => {
  expect(
    await addCommentsToCode(
      '',
      'code2',
      `function foo() {}`,
      [
        {
          loc: {
            start: {line: 1, column: 13, offset: 13},
            end: {line: 1, column: 14, offset: 14},
          },
          isError: true,
          lints: new Set(),
          error_codes: ['code1'],
        },
      ],
      flowBinPath,
    ),
  ).toEqual([`function foo() {}`, 0]);
});

test('addCommentsToCode > only suppresses errors matching the given code', async () => {
  expect(
    await addCommentsToCode(
      '',
      'code2',
      `function foo() {}
function bar<a>(): b {}
`,
      [
        {
          loc: {
            start: {line: 1, column: 13, offset: 13},
            end: {line: 1, column: 14, offset: 14},
          },
          isError: true,
          lints: new Set(),
          error_codes: ['code1'],
        },
        {
          loc: {
            start: {line: 2, column: 16, offset: 34},
            end: {line: 2, column: 17, offset: 35},
          },
          isError: true,
          lints: new Set(),
          error_codes: ['code2'],
        },
      ],
      flowBinPath,
    ),
  ).toEqual([
    `function foo() {}
// $FlowFixMe[code2]
function bar<a>(): b {}
`,
    1,
  ]);
});

test('addCommentsToCode > function return', async () => {
  expect(
    await addCommentsToCode(
      '',
      null,
      `
class A {
  foo(
    bar: string,
  ) {
    bar;
  }
}`,
      [
        {
          loc: {
            start: {line: 5, column: 3, offset: 38},
            end: {line: 5, column: 2, offset: 38},
          },
          isError: true,
          lints: new Set(),
          error_codes: ['code1'],
        },
      ],
      flowBinPath,
    ),
  ).toEqual([
    `
class A {
  foo(
    bar: string,
  // $FlowFixMe[code1]
  ) {
    bar;
  }
}`,
    1,
  ]);
});

test('addCommentsToCode > missing function return type', async () => {
  expect(
    await addCommentsToCode(
      '',
      null,
      `
class Foo {
  static methodBar = {};
}`,
      [
        {
          loc: {
            start: {line: 3, column: 19, offset: 31},
            end: {line: 3, column: 18, offset: 31},
          },
          isError: true,
          lints: new Set(),
          error_codes: ['code1'],
        },
      ],
      flowBinPath,
    ),
  ).toEqual([
    `
class Foo {
  // $FlowFixMe[code1]
  static methodBar = {};
}`,
    1,
  ]);
});

test('addCommentsToCode > JSX', async () => {
  expect(
    await addCommentsToCode(
      '',
      null,
      `function A(): React.Node {
  return (
    <div>
      <SomeComponent prop1={thing} />
      {thing}
      {thing}
      {
        // $FlowFixMe[code6]
        thing
      }
    </div>
  );
}`,
      [
        {
          loc: {
            start: {line: 4, column: 29, offset: 75},
            end: {line: 4, column: 33, offset: 80},
          },
          isError: true,
          lints: new Set(),
          error_codes: ['code1'],
        },
        {
          loc: {
            start: {line: 5, column: 8, offset: 93},
            end: {line: 5, column: 12, offset: 98},
          },
          isError: true,
          lints: new Set(),
          error_codes: ['code2', 'code3'],
        },
        {
          loc: {
            start: {line: 6, column: 8, offset: 107},
            end: {line: 6, column: 12, offset: 112},
          },
          isError: true,
          lints: new Set(),
          error_codes: ['code4'],
        },
        {
          loc: {
            start: {line: 6, column: 8, offset: 107},
            end: {line: 6, column: 12, offset: 112},
          },
          isError: true,
          lints: new Set(),
          error_codes: ['code5'],
        },
        {
          loc: {
            start: {line: 9, column: 9, offset: 159},
            end: {line: 9, column: 13, offset: 164},
          },
          isError: true,
          lints: new Set(),
          error_codes: ['code7'],
        },
      ],
      flowBinPath,
    ),
  ).toEqual([
    `function A(): React.Node {
  return (
    <div>
      {/* $FlowFixMe[code1] */}
      <SomeComponent prop1={thing} />
      {
        // $FlowFixMe[code3]
        // $FlowFixMe[code2]
        thing}
      {
        // $FlowFixMe[code5]
        // $FlowFixMe[code4]
        thing}
      {
        // $FlowFixMe[code6]
        // $FlowFixMe[code7]
        thing
      }
    </div>
  );
}`,
    6,
  ]);
});

const longComment =
  'this is a really long comment that definitely goes over the line length limit so the tool has to wrap it';
const testInput = `const bar = 4;
const foo = 4;
const baz = 3;
<>
  <div>
    <span>
      foo
    </span>
  </div>
</>
`;

const testOutput = `/* $FlowFixMe[code1] this is a really long comment that definitely goes over
 * the line length limit so the tool has to wrap it */
/* $FlowFixMe[code2] this is a really long comment that definitely goes over
 * the line length limit so the tool has to wrap it */
const bar = 4;
const foo = 4;
/* $FlowFixMe[code1] this is a really long comment that definitely goes over
 * the line length limit so the tool has to wrap it */
/* $FlowFixMe[code2] this is a really long comment that definitely goes over
 * the line length limit so the tool has to wrap it */
const baz = 3;
<>
  {/* $FlowFixMe[code1] this is a really long comment that definitely goes over
    * the line length limit so the tool has to wrap it */
   /* $FlowFixMe[code2] this is a really long comment that definitely goes over
    * the line length limit so the tool has to wrap it */}
  <div>
    {/* $FlowFixMe[code1] this is a really long comment that definitely goes
      * over the line length limit so the tool has to wrap it */
     /* $FlowFixMe[code2] this is a really long comment that definitely goes
      * over the line length limit so the tool has to wrap it */}
    <span>
      foo
    </span>
  </div>
</>
`;

// This simulates an error location spanning the entire line. The code looks for AST nodes that are
// completely contained by the error location, so this location goes from column 1 to a ridiculously
// large column number.
function makeSuppression(line, text) {
  return {
    loc: locForLine(line, text),
    isError: true,
    lints: new Set(),
    error_codes: ['code2', 'code1'],
  };
}

function locForLine(line, text) {
  let startOffset = 0;
  const lines = text.split('\n');
  for (let currLine = 0; currLine < line - 1; currLine++) {
    // Add the line length + \n to the offset
    startOffset += lines[currLine].length + 1;
  }
  const contentLength = lines[line - 1].trimLeft().length;
  const lineLength = lines[line - 1].length;
  return {
    start: {offset: startOffset, line, column: lineLength - contentLength},
    end: {
      offset: startOffset + lineLength,
      line,
      column: lineLength - 1,
    },
  };
}

test('removeUnusedErrorSuppressionsFromText', async () => {
  const flowBinPath = path.resolve(process.env.FLOW_BIN);

  const testInput = `   // single line comment with some whitespace
const bar = 4;
  /* multiline
 * comment with some whitespace */
const foo = 4;
<div>
  {/* comment
    * inside jsx */}
  <span>
    {/* another comment inside jsx, with trailing whitespace */ }
    foo
  </span>
  { /* and yet another, with leading whitespace */}
</div>
`;

  const testOutput = `const bar = 4;
const foo = 4;
<div>
  <span>
    foo
  </span>
</div>
`;

  const errorLocs = [
    // Deliberately make these out of order to test that this is handled properly
    [3, 3, 4, 35],
    [10, 6, 10, 64],
    [1, 4, 1, 47],
    [13, 5, 13, 51],
    [7, 4, 8, 20],
    [],
  ].map(args => makeLoc(testInput, ...args));
  await expectCommentsAreRemoved(testInput, testOutput, errorLocs, flowBinPath);
});

test('removeExtraSpaceWhenRemovingUnusedFlowLint', async () => {
  const flowBinPath = path.resolve(process.env.FLOW_BIN);

  const testInput = `// flowlint foo bar
//flowlint foo bar
// flowlint foo bar
// flowlint-next-line foo bar
//flowlint-next-line foo bar
// flowlint-next-line foo bar
1*1; // flowlint-line foo bar
1*1; //flowlint-line foo bar
1*1; // flowlint-line foo bar
`;

  const testOutput = `// flowlint bar
//flowlint bar
// flowlint foo
// flowlint-next-line bar
//flowlint-next-line bar
// flowlint-next-line foo
1*1; // flowlint-line bar
1*1; //flowlint-line bar
1*1; // flowlint-line foo
`;

  const errorLocs = [
    [1, 13, 1, 16],
    [2, 12, 2, 15],
    [3, 17, 3, 20],
    [4, 23, 4, 26],
    [5, 22, 5, 25],
    [6, 27, 6, 30],
    [7, 23, 7, 26],
    [8, 22, 8, 25],
    [9, 27, 9, 30],
  ].map(args => makeLoc(testInput, ...args));
  await expectCommentsAreRemoved(testInput, testOutput, errorLocs, flowBinPath);
});

test('deleteUnusedFlowLintComments', async () => {
  const flowBinPath = path.resolve(process.env.FLOW_BIN);

  const testInput = `// flowlint foo bar
//flowlint foo bar
// flowlint-next-line foo bar
//flowlint-next-line foo bar
1*1; // flowlint-line foo bar
1*1; //flowlint-line foo bar
/* flowlint foo bar */
/*flowlint foo bar */
/* flowlint-next-line foo bar */
/*flowlint-next-line foo bar */
1*1; /* flowlint-line foo bar */
1*1; /*flowlint-line foo bar */
<div>{/* flowlint-line foo */}</div>;
<div>
  {/* flowlint-line foo */}
</div>;
let x =
  { /* flowlint foo */ };
{ /* flowlint foo */ }
/* flowlint
 *   foo
 */
`;

  const testOutput = `1*1;
1*1;
1*1;
1*1;
<div></div>;
<div>
</div>;
let x =
  {  };
{  }
`;

  const errorLocs = [
    [1, 13, 1, 16],
    [1, 17, 1, 20],
    [2, 12, 2, 15],
    [2, 16, 2, 19],
    [3, 23, 3, 26],
    [3, 27, 3, 30],
    [4, 22, 4, 25],
    [4, 26, 4, 29],
    [5, 23, 5, 26],
    [5, 27, 5, 30],
    [6, 22, 6, 25],
    [6, 26, 6, 29],
    [7, 13, 7, 16],
    [7, 17, 7, 20],
    [8, 12, 8, 15],
    [8, 16, 8, 19],
    [9, 23, 9, 26],
    [9, 27, 9, 30],
    [10, 22, 10, 25],
    [10, 26, 10, 29],
    [11, 23, 11, 26],
    [11, 27, 11, 30],
    [12, 22, 12, 25],
    [12, 26, 12, 29],
    [13, 24, 13, 27],
    [15, 21, 15, 24],
    [18, 17, 18, 20],
    [19, 15, 19, 18],
    [21, 6, 21, 9],
  ].map(args => makeLoc(testInput, ...args));
  await expectCommentsAreRemoved(testInput, testOutput, errorLocs, flowBinPath);
});

test('unicode', async () => {
  const flowBinPath = path.resolve(process.env.FLOW_BIN);

  const testInput = `const unicode = "\u{714E}\u{8336}";
const smiley = "\uD83D\uDE00";

// flowlint foo bar
//flowlint foo bar
// flowlint-next-line foo bar
//flowlint-next-line foo bar
1*1; // flowlint-line foo bar
1*1; //flowlint-line foo bar
/* flowlint foo bar */
/*flowlint foo bar */
/* flowlint-next-line foo bar */
/*flowlint-next-line foo bar */
1*1; /* flowlint-line foo bar */
1*1; /*flowlint-line foo bar */
<div>{/* flowlint-line foo */}</div>;
<div>
  {/* flowlint-line foo */}
</div>;
let x =
  { /* flowlint foo */ };
{ /* flowlint foo */ }
`;

  const testOutput = `const unicode = "\u{714E}\u{8336}";
const smiley = "\uD83D\uDE00";

1*1;
1*1;
1*1;
1*1;
<div></div>;
<div>
</div>;
let x =
  {  };
{  }
`;

  const errorLocs = [
    [4, 13, 4, 16],
    [4, 17, 4, 20],
    [5, 12, 5, 15],
    [5, 16, 5, 19],
    [6, 23, 6, 26],
    [6, 27, 6, 30],
    [7, 22, 7, 25],
    [7, 26, 7, 29],
    [8, 23, 8, 26],
    [8, 27, 8, 30],
    [9, 22, 9, 25],
    [9, 26, 9, 29],
    [10, 13, 10, 16],
    [10, 17, 10, 20],
    [11, 12, 11, 15],
    [11, 16, 11, 19],
    [12, 23, 12, 26],
    [12, 27, 12, 30],
    [13, 22, 13, 25],
    [13, 26, 13, 29],
    [14, 23, 14, 26],
    [14, 27, 14, 30],
    [15, 22, 15, 25],
    [15, 26, 15, 29],
    [16, 24, 16, 27],
    [18, 21, 18, 24],
    [21, 17, 21, 20],
    [22, 15, 22, 18],
  ].map(args => makeLoc(testInput, ...args));
  await expectCommentsAreRemoved(testInput, testOutput, errorLocs, flowBinPath);
});

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

async function expectCommentsAreRemoved(
  input,
  expectedOutput,
  errorLocs,
  flowBinPath,
) {
  const actualOutput = await removeUnusedErrorSuppressionsFromText(
    Buffer.from(input),
    errorLocs,
    flowBinPath,
  );
  expect(actualOutput.toString()).toEqual(expectedOutput);
}

(async () => {
  for (const {name, fn} of collectedTests) {
    try {
      await fn();
      console.error(`[SUCCESS] ${name}`);
    } catch (e) {
      console.error(`[FAILURE] ${name}`);
      console.error(e);
      throw e;
    }
  }
})();
