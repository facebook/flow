/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const babel = require('@babel/core');
const Module = require('module');

const srcDir = require('path').resolve(__dirname, '..');

const originalCompile = Module.prototype._compile;
Module.prototype._compile = function (code, filename) {
  if (filename.startsWith(srcDir) && filename.endsWith('.js')) {
    const result = babel.transformSync(code, {
      filename,
      presets: ['@babel/preset-flow'],
      plugins: ['babel-plugin-syntax-hermes-parser'],
    });
    code = result.code;
  }
  return originalCompile.call(this, code, filename);
};

const path = require('path');
const {spawnSync} = require('child_process');

const {exec} = require('../utils/async');
const {splitIntoChunks} = require('../utils/string');
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
          error_codes: ['code1'],
        },
        {
          loc: {
            start: {line: 2, column: 16, offset: 34},
            end: {line: 2, column: 17, offset: 35},
          },
          error_codes: ['code2'],
        },
        {
          loc: {
            start: {line: 3, column: 14, offset: 56},
            end: {line: 3, column: 15, offset: 57},
          },
          error_codes: ['code3'],
        },
        {
          loc: {
            start: {line: 4, column: 5, offset: 71},
            end: {line: 4, column: 6, offset: 72},
          },
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
          error_codes: ['code1'],
        },
        {
          loc: {
            start: {line: 2, column: 16, offset: 34},
            end: {line: 2, column: 17, offset: 35},
          },
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
          error_codes: ['code1'],
        },
        {
          loc: {
            start: {line: 5, column: 8, offset: 93},
            end: {line: 5, column: 12, offset: 98},
          },
          error_codes: ['code2', 'code3'],
        },
        {
          loc: {
            start: {line: 6, column: 8, offset: 107},
            end: {line: 6, column: 12, offset: 112},
          },
          error_codes: ['code4'],
        },
        {
          loc: {
            start: {line: 6, column: 8, offset: 107},
            end: {line: 6, column: 12, offset: 112},
          },
          error_codes: ['code5'],
        },
        {
          loc: {
            start: {line: 9, column: 9, offset: 159},
            end: {line: 9, column: 13, offset: 164},
          },
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
