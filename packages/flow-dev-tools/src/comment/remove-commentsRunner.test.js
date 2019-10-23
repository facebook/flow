/*
 * @format
 */

import {removeUnusedErrorSuppressionsFromText} from './remove-commentsRunner';

import * as path from 'path';

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
  expectCommentsAreRemoved(testInput, testOutput, errorLocs, flowBinPath);
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
  expectCommentsAreRemoved(testInput, testOutput, errorLocs, flowBinPath);
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
  ].map(args => makeLoc(testInput, ...args));
  expectCommentsAreRemoved(testInput, testOutput, errorLocs, flowBinPath);
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
  expectCommentsAreRemoved(testInput, testOutput, errorLocs, flowBinPath);
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

function posToOffset(contents: string, line: number, col: number) {
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
