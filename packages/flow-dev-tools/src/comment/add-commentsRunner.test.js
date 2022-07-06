/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const {addCommentsToCode} = require('./add-commentsRunner');

const path = require('path');

test('addCommentsToCode', async () => {
  const flowBinPath = path.resolve(process.env.FLOW_BIN);

  expect(await addCommentsToCode('foobar', null, '', [], flowBinPath)).toEqual([
    '',
    0,
  ]);

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
    * the line length limit so the tool has to wrap it */}
  {/* $FlowFixMe[code2] this is a really long comment that definitely goes over
    * the line length limit so the tool has to wrap it */}
  <div>
    {/* $FlowFixMe[code1] this is a really long comment that definitely goes
      * over the line length limit so the tool has to wrap it */}
    {/* $FlowFixMe[code2] this is a really long comment that definitely goes
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
  const [startOffset, endOffset] = offsetsForLine(line, text);
  return {
    loc: {
      start: {
        line,
        column: 1,
        offset: startOffset,
      },
      end: {
        line,
        column: 10000,
        offset: endOffset,
      },
    },
    isError: true,
    lints: new Set(),
    error_codes: ['code2', 'code1'],
  };
}

function offsetsForLine(line, text) {
  let startOffset = 0;
  const lines = text.split('\n');
  for (let currLine = 0; currLine < line - 1; currLine++) {
    // Add the line length + \n to the offset
    startOffset += lines[currLine].length + 1;
  }
  return [startOffset, startOffset + lines[line - 1].length];
}
