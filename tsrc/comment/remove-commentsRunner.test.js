import {removeUnusedErrorSuppressionsFromText} from './remove-commentsRunner';

test('removeUnusedErrorSuppressionsFromText', () => {
  const errorLocs = [
    // Deliberately make these out of order to test that this is handled properly
    [3, 3, 4, 35],
    [1, 4, 1, 47],
    [7, 5, 8, 20],
  ].map((args) => makeLoc(testInput, ...args));
  expect(removeUnusedErrorSuppressionsFromText(testInput, errorLocs)).toEqual(testOutput);
});

const testInput =
`   // single line comment with some whitespace
const bar = 4;
  /* multiline
 * comment with some whitespace */
const foo = 4;
<div>
  { /* comment
    * inside jsx */  }
  <span>
    foo
  </span>
</div>
`;


// There are a couple oddities about this -- first, the removal of the first comment leaves a
// newline around. This is probably an edge case related to removing the first line in the file.
// Second, the whitespace surrounding the JSX comment is not removed, even though it probably should
// be (along with the curly braces).
const testOutput =
`
const bar = 4;
const foo = 4;
<div>
  {   }
  <span>
    foo
  </span>
</div>
`;

function makeLoc(contents, startLine, startCol, endLine, endCol) {
  return {
    start: posToOffset(contents, startLine, startCol),
    end: posToOffset(contents, endLine, endCol),
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
