/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

// $FlowExpectedError[cannot-resolve-module]
import prettierConfig from '../../.prettierrc.json';

import * as prettier from 'prettier';

function getOptions() {
  return {
    ...prettierConfig,
    parser: 'hermes',
    requirePragma: false,
    plugins: [require.resolve('../index.mjs')],
  };
}

describe('Component Declaration', () => {
  it('Formatting', async () => {
    const code = `
component MyComponent() {}

component MyComponent() renders SomeComponent {}

component MyComponent() renders React.Element<typeof SomeComponentLonnnnnnnnnnnnnnnnnnnnnnnnnnnnng> {}

component MyComponent() {
  return <OtherComponent />;
}

export component MyComponent() {}

export default component MyComponent() {}

component MyComponent<T>() {}

component MyComponent<T: Fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo>() {}

component MyComponent(bar: string) {}

component MyComponent(bar?: string) {}

component MyComponent(bar: string = '') {}

component MyComponent(propBar as bar: string) {}

component MyComponent(propBar as [bar]: $ReadOnlyArray<string>) {}

component MyComponent(propBar as {bar}: $ReadOnly<{bar: string}>) {}

component MyComponent(propBar as {bar, reallllllllllllllllllllllllllllllllllllllyLong}: $ReadOnly<{bar: string, reallllllllllllllllllllllllllllllllllllllyLong: string}>) {}

component MyComponent('data-bar' as bar: string) {}

component MyComponent(...restProps: $ReadOnly<{k: string}>) {}

component MyComponent(bar: string, baz: $ReadOnly<{k: string}>) {}

component MyComponent(bar: string, baz: $ReadOnly<{k: string}>, realllllllllllllllllllyLong: string) {}

component MyComponent(bar: string, baz: $ReadOnly<{k: string, reallllllllllllllllllllllllllllllllllllllyLong: string}>) {}

component MyComponent(bar: string, 'data-baz' as baz: $ReadOnly<{k: string, reallllllllllllllllllllllllllllllllllllllyLong: string}>) {}

// Attached comment
component MyComponent(
  /**
   * Commet block
   */
  bar: string, // Trailing comment

  // preceding comment
  'data-baz' as baz: $ReadOnly<{k: string, reallllllllllllllllllllllllllllllllllllllyLong: string}>
  // Trailing comment
) {}

component MyComponent(
  ...props: $ReadOnly<{k: string, reallllllllllllllllllllllllllllllllllllllyLong: string}>
  // Trailing comment
) {}

component MyComponent() /* Trailing comment */ {}
    `;
    const output = await prettier.format(code, getOptions());
    expect(output).toMatchInlineSnapshot(`
      "component MyComponent() {}

      component MyComponent() renders SomeComponent {}

      component MyComponent() renders React.Element<
        typeof SomeComponentLonnnnnnnnnnnnnnnnnnnnnnnnnnnnng,
      > {}

      component MyComponent() {
        return <OtherComponent />;
      }

      export component MyComponent() {}

      export default component MyComponent() {}

      component MyComponent<T>() {}

      component MyComponent<
        T: Fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo,
      >() {}

      component MyComponent(bar: string) {}

      component MyComponent(bar?: string) {}

      component MyComponent(bar: string = '') {}

      component MyComponent(propBar as bar: string) {}

      component MyComponent(propBar as [bar]: $ReadOnlyArray<string>) {}

      component MyComponent(propBar as {bar}: $ReadOnly<{bar: string}>) {}

      component MyComponent(
        propBar as {
          bar,
          reallllllllllllllllllllllllllllllllllllllyLong,
        }: $ReadOnly<{
          bar: string,
          reallllllllllllllllllllllllllllllllllllllyLong: string,
        }>,
      ) {}

      component MyComponent('data-bar' as bar: string) {}

      component MyComponent(...restProps: $ReadOnly<{k: string}>) {}

      component MyComponent(bar: string, baz: $ReadOnly<{k: string}>) {}

      component MyComponent(
        bar: string,
        baz: $ReadOnly<{k: string}>,
        realllllllllllllllllllyLong: string,
      ) {}

      component MyComponent(
        bar: string,
        baz: $ReadOnly<{
          k: string,
          reallllllllllllllllllllllllllllllllllllllyLong: string,
        }>,
      ) {}

      component MyComponent(
        bar: string,
        'data-baz' as baz: $ReadOnly<{
          k: string,
          reallllllllllllllllllllllllllllllllllllllyLong: string,
        }>,
      ) {}

      // Attached comment
      component MyComponent(
        /**
         * Commet block
         */
        bar: string, // Trailing comment

        // preceding comment
        'data-baz' as baz: $ReadOnly<{
          k: string,
          reallllllllllllllllllllllllllllllllllllllyLong: string,
        }>,
        // Trailing comment
      ) {}

      component MyComponent(
        ...props: $ReadOnly<{
          k: string,
          reallllllllllllllllllllllllllllllllllllllyLong: string,
        }>
        // Trailing comment
      ) {}

      component MyComponent() /* Trailing comment */ {}
      "
    `);
  });
});
