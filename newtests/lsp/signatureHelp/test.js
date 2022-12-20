/*
 * @flow
 */

import type {Suite} from "../../../packages/flow-dev-tools/src/test/Suite";
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(
  ({
    addFile,
    addFiles,
    addCode,
    lspStartAndConnect,
    lspRequestAndWaitUntilResponse,
    lspIgnoreStatusAndCancellation,
  }) => {
    let verifySignatureHelp = function(
      uri: string,
      position: {|line: number, character: number|},
      response: string,
    ) {
      return lspRequestAndWaitUntilResponse('textDocument/signatureHelp', {
        textDocument: {uri: uri},
        position: position,
      }).verifyAllLSPMessagesInStep(
        [['textDocument/signatureHelp', response]],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      );
    };
    return [
      test('textDocument/signatureHelp', [
        addFiles(
          'nestedClasses.js',
          'nestedFunctions.js',
          'paramDocumentation.js',
          'tupleRestParam.js',
        ),
        lspStartAndConnect(),
        verifySignatureHelp(
          '<PLACEHOLDER_PROJECT_URL>/nestedClasses.js',
          {line: 4, character: 2}, // `f(|class {})`
          JSON.stringify({
            signatures: [
              {label: '(x: mixed): void', parameters: [{label: 'x: mixed'}]},
            ],
            activeSignature: 0,
            activeParameter: 0,
          }),
        ),
        verifySignatureHelp(
          '<PLACEHOLDER_PROJECT_URL>/nestedClasses.js',
          {line: 4, character: 7}, // `f(class| {})`
          JSON.stringify({
            signatures: [
              {label: '(x: mixed): void', parameters: [{label: 'x: mixed'}]},
            ],
            activeSignature: 0,
            activeParameter: 0,
          }),
        ),
        verifySignatureHelp(
          '<PLACEHOLDER_PROJECT_URL>/nestedClasses.js',
          {line: 4, character: 9}, // `f(class {|})`
          '{null}',
        ),
        verifySignatureHelp(
          '<PLACEHOLDER_PROJECT_URL>/nestedFunctions.js',
          {line: 6, character: 2},
          JSON.stringify({
            signatures: [
              {
                label: '(a: string, f: F, b: number): void',
                parameters: [
                  {label: 'a: string'},
                  {label: 'f: F'},
                  {label: 'b: number'},
                ],
              },
            ],
            activeSignature: 0,
            activeParameter: 0,
          }),
        ),
        verifySignatureHelp(
          '<PLACEHOLDER_PROJECT_URL>/nestedFunctions.js',
          {line: 8, character: 2}, // inside nested function body
          '{null}',
        ),
        verifySignatureHelp(
          '<PLACEHOLDER_PROJECT_URL>/tupleRestParam.js',
          {line: 6, character: 1},
          JSON.stringify({
            signatures: [
              {
                label: '(arg[0]: string, arg[1]: number): void',
                parameters: [
                  {label: 'arg[0]: string'},
                  {label: 'arg[1]: number'},
                ],
              },
            ],
            activeSignature: 0,
            activeParameter: 0,
          }),
        ),
        verifySignatureHelp(
          '<PLACEHOLDER_PROJECT_URL>/tupleRestParam.js',
          {line: 12, character: 1},
          JSON.stringify({
            signatures: [
              {
                label: '(params[0]: string, params[1]: number): void',
                parameters: [
                  {label: 'params[0]: string'},
                  {label: 'params[1]: number'},
                ],
              },
            ],
            activeSignature: 0,
            activeParameter: 0,
          }),
        ),
        verifySignatureHelp(
          '<PLACEHOLDER_PROJECT_URL>/tupleRestParam.js',
          {line: 18, character: 1},
          JSON.stringify({
            signatures: [
              {
                label: '(foo: string, bar: number): void',
                parameters: [
                  {label: 'foo: string'},
                  {label: 'bar: number'},
                ],
              },
            ],
            activeSignature: 0,
            activeParameter: 0,
          }),
        ),
        verifySignatureHelp(
          '<PLACEHOLDER_PROJECT_URL>/tupleRestParam.js',
          {line: 24, character: 1},
          JSON.stringify({
            signatures: [
              {
                label: '(foo: string, bar: number): void',
                parameters: [
                  {label: 'foo: string'},
                  {label: 'bar: number'},
                ],
              },
            ],
            activeSignature: 0,
            activeParameter: 0,
          }),
        ),
        verifySignatureHelp(
          '<PLACEHOLDER_PROJECT_URL>/tupleRestParam.js',
          {line: 30, character: 1},
          JSON.stringify({
            signatures: [
              {
                label: '(foo: string, arg[1]: number): void',
                parameters: [
                  {label: 'foo: string'},
                  {label: 'arg[1]: number'},
                ],
              },
            ],
            activeSignature: 0,
            activeParameter: 0,
          }),
        ),
        verifySignatureHelp(
          '<PLACEHOLDER_PROJECT_URL>/tupleRestParam.js',
          {line: 36, character: 1},
          JSON.stringify({
            signatures: [
              {
                label: '(foo: string, params[1]: number): void',
                parameters: [
                  {label: 'foo: string'},
                  {label: 'params[1]: number'},
                ],
              },
            ],
            activeSignature: 0,
            activeParameter: 0,
          }),
        ),
        verifySignatureHelp(
          '<PLACEHOLDER_PROJECT_URL>/paramDocumentation.js',
          {line: 12, character: 5},
          JSON.stringify({
            signatures: [
              {
                label: '(bar: void, baz: void): void',
                documentation: {
                  kind: 'markdown',
                  value: 'foo\n\n**@unrecognized** this tag is unrecognized',
                },
                parameters: [
                  {
                    label: 'bar: void',
                    documentation: {
                      kind: 'markdown',
                      value: 'bar - the first summand',
                    },
                  },
                  {
                    label: 'baz: void',
                    documentation: {
                      kind: 'markdown',
                      value:
                        'baz - the second and third summands\nbaz.x (optional)  - the second summand\nbaz.y (optional, defaults to 0)  - the third summand',
                    },
                  },
                ],
              },
            ],
            activeSignature: 0,
            activeParameter: 0,
          }),
        ),
      ]),
    ];
  },
): Suite);
