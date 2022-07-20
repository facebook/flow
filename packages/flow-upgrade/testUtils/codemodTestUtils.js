/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import type {Codemod} from '../src/Types';
import type {TransformContext} from 'hermes-transform';

import {redirectConsole, restoreConsole} from '../src/utils/redirectConsole';
import babelHighlight from '@babel/highlight';
import {transform as hermesTransform} from 'hermes-transform';

type AnyObject = $ReadOnly<{...}>;

// use a consistent, static prettier config instead of loading some root config
// so that snapshots won't break if the config changes
const prettierConfig = Object.freeze({
  arrowParens: 'avoid',
  singleQuote: true,
  trailingComma: 'all',
  bracketSpacing: false,
  bracketSameLine: true,
});

type CodemodTest<TCliArgs: ?AnyObject> = $ReadOnly<{
  /** a description to add before the test to make it clearer what it's testing */
  description: string,
  /** the code to test transform */
  code: string,
  /** if true, then all other tests will be skipped and only this will be run */
  only?: true,
  /** cli arguments to "pass" for the test */
  cliArgs?: TCliArgs,
}>;

type CodemodTestWithConfig<
  TConfig: ?AnyObject,
  TCliArgs: ?AnyObject,
> = $ReadOnly<{
  ...CodemodTest<TCliArgs>,
  /** config for this test case */
  config?: TConfig,
  /** optionally a filename to pass to the transform config function. If not provided will just pass `test.js` */
  filename?: string,
}>;
export type CodemodTestWithConfigWithOutput<
  TConfig: ?AnyObject,
  TCliArgs: ?AnyObject,
> = $ReadOnly<{
  ...CodemodTestWithConfig<TConfig, TCliArgs>,
  /** the expected transform output */
  output: string,
}>;

export function testCodemod<TConfig: ?AnyObject, TCliArgs: ?AnyObject>(
  codemodName: string,
  codemodModule: Codemod,
  tests: $ReadOnly<{
    ignored: $ReadOnlyArray<CodemodTestWithConfig<TConfig, TCliArgs>>,
    transformed: $ReadOnlyArray<
      CodemodTestWithConfigWithOutput<TConfig, TCliArgs>,
    >,
  }>,
): void {
  describe(codemodName, () => {
    // TODO(bradzacher) - maybe we should allow the test to specify the expected log lines?
    beforeEach(() => {
      redirectConsole({
        onStdout() {},
        onStderr() {},
      });
    });
    afterEach(() => {
      restoreConsole();
    });

    let hasOnly = false;
    for (const test of tests.ignored.concat(tests.transformed)) {
      if (test.only) {
        hasOnly = true;
        break;
      }
    }

    function testTransform(
      test: CodemodTestWithConfigWithOutput<TConfig, TCliArgs>,
    ) {
      const code = test.code.trim();
      const cliArgs = test.cliArgs ?? {};

      if (hasOnly && test.only !== true) {
        // jest will still print the skipped test cases (and thus the code
        // fragments) which just creates noise in the console.
        //
        // to reduce noise we don't print the code fragment, just the description
        it(test.description, () => {
          expect('skipped').toBe('skipped');
        });
        return;
      }

      (test.only === true ? it.only : it)(
        [
          // the newline before forces the code onto a new line which makes sure
          // that multi-line test output aligns properly
          '',
          `// ${test.description}`,
          babelHighlight(code),
          // the newline after breaks the test output up so it's easier to see
          // jest's tick/cross
          '',
          '',
        ].join('\n'),
        () => {
          const transform = (() => {
            return (ctx: TransformContext) =>
              // $FlowFixMe[incompatible-call]
              // $FlowFixMe[incompatible-exact]
              // $FlowFixMe[prop-missing]
              codemodModule.transform(ctx);
          })();
          const result = hermesTransform(code, transform, prettierConfig);
          expect(result.trim()).toEqual(test.output.trim());
        },
      );
    }

    describe('ignored', () => {
      for (const test of tests.ignored) {
        testTransform({...test, output: test.code});
      }
    });

    describe('transformed', () => {
      for (const test of tests.transformed) {
        testTransform(test);
      }
    });
  });
}
