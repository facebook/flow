import {removeUnusedErrorSuppressionsFromText} from './remove-commentsRunner';

import * as path from 'path';

test('removeUnusedErrorSuppressionsFromText', async () => {
  const flowBinPath = path.resolve(process.env.FLOW_BIN);

  const errorLocs = [
    // Deliberately make these out of order to test that this is handled properly
    [3, 3, 4, 35],
    [10, 6, 10, 64],
    [1, 4, 1, 47],
    [13, 5, 13, 51],
    [7, 4, 8, 20],
  ].map((args) => makeLoc(testInput, ...args));
  expect(await removeUnusedErrorSuppressionsFromText(testInput, errorLocs, flowBinPath)).toEqual(testOutput);
});

const testInput =
`   // single line comment with some whitespace
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


// It's odd that the removal of the first comment leaves the empty line around. I suspect it's just
// an edge case that occurs only when there is a comment on the first line. Probably not worth
// fixing since that's a pretty rare occurrence.
const testOutput =
`
const bar = 4;
const foo = 4;
<div>
  <span>
    { }
    foo
  </span>
  { }
</div>
`;

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

function posToOffset(contents: string, line: number, col: number) {
  let offset = 0;
  // Using 1-indexed line and column for this
  let currentLine = 1;
  let currentCol = 1;
  while (offset < contents.length && !(currentLine === line && currentCol === col)) {
    const char = contents[offset];
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
