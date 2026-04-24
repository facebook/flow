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

describe('Hook Declaration', () => {
  it('Formatting', async () => {
    const code = `
    hook useFoo1() {}

    export default hook useFoo2() {}

    export hook useFoo3() {}

    hook useFoo4(): string {}

    hook useFoo5<T>() {}

    hook useFoo6(...foo) {}

    hook useFoo7(...rest?: Foo) {}

    hook useFoo8(foo, ...bar) {}

    hook useFoo9(foo: Foo, ...bar: Bar) {}

    hook useFoo10(foo: () => void,): number { return; }

    hook useFoo11(o: { f(string): void }) {}

    hook useFoo12(foo, ...bar): React.Element<typeof SomeComponentLonnnnnnnnnnnnnnnnnnnnnnnnnnnnng> {}

    hook useFoo13(foo: Array<Fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo>, ...bar): void {}

    hook useFoo14<
    T: Fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo,
  >(): any {};
    `;
    const output = await prettier.format(code, getOptions());
    expect(output).toMatchInlineSnapshot(`
      "hook useFoo1() {}

      export default hook useFoo2() {}

      export hook useFoo3() {}

      hook useFoo4(): string {}

      hook useFoo5<T>() {}

      hook useFoo6(...foo) {}

      hook useFoo7(...rest?: Foo) {}

      hook useFoo8(foo, ...bar) {}

      hook useFoo9(foo: Foo, ...bar: Bar) {}

      hook useFoo10(foo: () => void): number {
        return;
      }

      hook useFoo11(o: {f(string): void}) {}

      hook useFoo12(
        foo,
        ...bar
      ): React.Element<typeof SomeComponentLonnnnnnnnnnnnnnnnnnnnnnnnnnnnng> {}

      hook useFoo13(
        foo: Array<Fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo>,
        ...bar
      ): void {}

      hook useFoo14<
        T: Fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo,
      >(): any {}
      "
    `);
  });
});
