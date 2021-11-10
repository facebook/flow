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
        `
          {
            "path": "",
            "line": 0,
            "endline": 0,
            "start": 1,
            "end": 0
          }
        `,
      )
      .because('Should not work because directory is ignored'),
    flowCmd(
      ['type-at-pos', '--strip-root', '--json', 'ignore/foo.js', '3', '2'],
    )
      .stdout(
        `
          {
            "type": "(unknown)",
            "reasons": [],
            "loc": {
              "source": null,
              "type": null,
              "start": {
                "line": 0,
                "column": 1,
                "offset": 0
              },
              "end": {
                "line": 0,
                "column": 0,
                "offset": 0
              }
            },
            "path": "",
            "line": 0,
            "endline": 0,
            "start": 1,
            "end": 0
          }
        `,
      )
      .because('Should not work when directory is ignored'),
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
        `
          {
            "path": "",
            "line": 0,
            "endline": 0,
            "start": 1,
            "end": 0
          }
        `,
      )
      .because('Should not work because no_flow/foo.js is missing @flow'),
    flowCmd(
      ['type-at-pos', '--strip-root', '--json', 'no_flow/foo.js', '3', '2'],
    )
      .stdout(
        `
          {
            "type": "(unknown)",
            "reasons": [],
            "loc": {
              "source": null,
              "type": null,
              "start": {
                "line": 0,
                "column": 1,
                "offset": 0
              },
              "end": {
                "line": 0,
                "column": 0,
                "offset": 0
              }
            },
            "path": "",
            "line": 0,
            "endline": 0,
            "start": 1,
            "end": 0
          }
        `,
      )
      .because('Should not work because no_flow/foo.js is missing @flow'),
  ]),
]): Suite);
