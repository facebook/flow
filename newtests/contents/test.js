/*
 * @flow
 */


import type {Suite} from "flow-dev-tools/src/test/Suite";
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(({addFiles, flowCmd}) => [
  test('Ignored directory', [
    addFiles(
      'ignore/dummy.js',
      'ignore/foo.js',
    ),
    flowCmd(
      ['get-def', '--strip-root', '--json', 'ignore/foo.js', '3', '2'],
    )
      .stdout(
`{
  "path": "ignore/foo.js",
  "line": 2,
  "endline": 2,
  "start": 5,
  "end": 7
}`,
)
      .because('Should work even though directory is ignored'),
    flowCmd(
      ['type-at-pos', '--strip-root', '--json', 'ignore/foo.js', '3', '2'],
    )
      .stdout(
        `
          {
            "type": "number",
            "reasons": [],
            "loc": {
              "source": "ignore/foo.js",
              "type": "SourceFile",
              "start": {
                "line": 3,
                "column": 1,
                "offset": 33
              },
              "end": {
                "line": 3,
                "column": 3,
                "offset": 36
              }
            },
            "path": "ignore/foo.js",
            "line": 3,
            "endline": 3,
            "start": 1,
            "end": 3
          }
        `,
      )
      .because('Should work even though directory is ignored'),
  ]),

  test('Directory with no @flow', [
    addFiles(
      'no_flow/dummy.js',
      'no_flow/foo.js',
    ),
    flowCmd(
      ['get-def', '--strip-root', '--json', 'no_flow/foo.js', '3', '2'],
    )
      .stdout(
`{
  "path": "no_flow/foo.js",
  "line": 2,
  "endline": 2,
  "start": 5,
  "end": 7
}`,
)
      .because('Should work even though no_flow/foo.js is missing @flow'),
    flowCmd(
      ['type-at-pos', '--strip-root', '--json', 'no_flow/foo.js', '3', '2'],
    )
      .stdout(
        `
          {
            "type": "number",
            "reasons": [],
            "loc": {
              "source": "no_flow/foo.js",
              "type": "SourceFile",
              "start": {
                "line": 3,
                "column": 1,
                "offset": 33
              },
              "end": {
                "line": 3,
                "column": 3,
                "offset": 36
              }
            },
            "path": "no_flow/foo.js",
            "line": 3,
            "endline": 3,
            "start": 1,
            "end": 3
          }
        `,
      )
      .because('Should work even though no_flow/foo.js is missing @flow'),
  ]),
]): Suite);
