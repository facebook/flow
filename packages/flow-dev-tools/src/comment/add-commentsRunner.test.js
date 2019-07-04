import {addCommentsToCode} from './add-commentsRunner.js';

import * as path from 'path';

test('addCommentsToCode', async () => {
  const flowBinPath = path.resolve(process.env.FLOW_BIN);

  expect(await addCommentsToCode('foobar', '', [], flowBinPath)).toEqual(['', 0]);

  expect(
    await addCommentsToCode(
      longComment,
      testInput,
      /* Intentionally made these out of order to test that they are still inserted properly */
      [1, 6, 5, 3].map(makeLoc),
      flowBinPath,
    )
  ).toEqual([
    testOutput,
    4
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
`/* this is a really long comment that definitely goes over the line length
 * limit so the tool has to wrap it */
const bar = 4;
const foo = 4;
/* this is a really long comment that definitely goes over the line length
 * limit so the tool has to wrap it */
const baz = 3;
<>
  {/* this is a really long comment that definitely goes over the line length
    * limit so the tool has to wrap it */}
  <div>
    {/* this is a really long comment that definitely goes over the line length
      * limit so the tool has to wrap it */}
    <span>
      foo
    </span>
  </div>
</>
`;

// This simulates an error location spanning the entire line. The code looks for AST nodes that are
// completely contained by the error location, so this location goes from column 1 to a ridiculously
// large column number.
function makeLoc(line: number) {
  return {
    start: {
      line,
      column: 1,
      offset: 0,
    },
    end: {
      line,
      column: 10000,
      offset: 0,
    },
  };
}
