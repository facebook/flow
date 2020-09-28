/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import {addCommentsToCode} from './add-commentsRunner.js';
import type {Suppression} from './add-commentsRunner.js'

import * as path from 'path';

test('addCommentsToCode', async () => {
  const flowBinPath = path.resolve(process.env.FLOW_BIN);

  expect(await addCommentsToCode('foobar', null, '', [], flowBinPath)).toEqual(['', 0]);

  expect(
    await addCommentsToCode(
      longComment,
      null,
      testInput,
      /* Intentionally made these out of order to test that they are still inserted properly */
      [1, 6, 5, 3].map((line) => makeSuppression(line, testInput)),
      flowBinPath,
    )
  ).toEqual([
    testOutput,
    8
  ]);
});

const longComment = 'this is a really long comment that definitely goes over the line length limit so the tool has to wrap it';
const testInput =
`const bar = 4;
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

const testOutput =
`/* $FlowFixMe[code1] this is a really long comment that definitely goes over
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
function makeSuppression(line: number, text: string): Suppression {
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
    error_codes:["code2","code1"]
  };
}
function offsetsForLine(line: number, text: string): [number, number] {
  let startOffset = 0;
  const lines = text.split('\n');
  for (let currLine = 0; currLine < line - 1; currLine++ ) {
    // Add the line length + \n to the offset
    startOffset += lines[currLine].length + 1;
  }
  return [startOffset, startOffset + lines[line -1].length];
}
