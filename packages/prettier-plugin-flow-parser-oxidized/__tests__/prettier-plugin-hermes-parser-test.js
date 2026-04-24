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

import * as prettierV3 from 'prettier';

function getOptions() {
  return {
    ...prettierConfig,
    parser: 'hermes',
    requirePragma: false,
    plugins: [require.resolve('../index.mjs')],
  };
}

async function runTestWithPrettier(prettier: typeof prettierV3) {
  const code = `
  // Function with graphql embed
  function foo() {
    useMutation(
      graphql\`
        mutation FooMutation
        {foo}
      \`
    );
  }

  // Prettier ignore comments work
  const a = {
    ...foo.bar
      // prettier-ignore
      // $FlowFixMe[incompatible-use]
      .baz,
  }

  // Object with css embed
  const styles = {
    content: css\`
      column-gap:
      8px;
      display: grid;
      grid-template-columns: 1fr 3fr;
    \`,
    infoItem: css\`
      margin-bottom: 12px;
      overflow-wrap: break-anywhere;
    \`,
  };

  // TypePredicate
  function isString (x: mixed): x is string { return typeof x === "string"; }

  // ComponentDeclaration
  component Foo(
    bar: {
      // @deprecated
      baz?: Baz,
    },
  ) {}

  // HookDeclaration
  hook useFoo(
    bar: {
      // @deprecated
      baz?: Baz,
    },
  ) {}
`;
  const output = await prettier.format(code, getOptions());
  expect(output).toMatchInlineSnapshot(`
    "// Function with graphql embed
    function foo() {
      useMutation(graphql\`
        mutation FooMutation {
          foo
        }
      \`);
    }

    // Prettier ignore comments work
    const a = {
      ...foo.bar
          // prettier-ignore
          // $FlowFixMe[incompatible-use]
          .baz,
    };

    // Object with css embed
    const styles = {
      content: css\`
        column-gap: 8px;
        display: grid;
        grid-template-columns: 1fr 3fr;
      \`,
      infoItem: css\`
        margin-bottom: 12px;
        overflow-wrap: break-anywhere;
      \`,
    };

    // TypePredicate
    function isString(x: mixed): x is string {
      return typeof x === 'string';
    }

    // ComponentDeclaration
    component Foo(
      bar: {
        // @deprecated
        baz?: Baz,
      },
    ) {}

    // HookDeclaration
    hook useFoo(bar: {
      // @deprecated
      baz?: Baz,
    }) {}
    "
  `);
}

describe('prettier-plugin-hermes-parser', () => {
  it('uses plugin for v3', async () => {
    await runTestWithPrettier(prettierV3);
  });

  it('formats JSX variable declaration stably', async () => {
    const code = `const notFoundView = (
      <LumaView   style={styles.root}
      >{collectionHeader} </LumaView>
    );`;

    // First format
    const firstPass = await prettierV3.format(code, getOptions());

    // Assert the output
    expect(firstPass).toMatchInlineSnapshot(`
      "const notFoundView = (
        <LumaView style={styles.root}>{collectionHeader} </LumaView>
      );
      "
    `);

    // Second format - should be identical to first (stability check)
    const secondPass = await prettierV3.format(firstPass, getOptions());

    expect(secondPass).toBe(firstPass);
  });

  it('does not add spurious empty lines in nested JSX', async () => {
    const code = `
      function Foo() {
        return (
          <View style={styles.root}>
            <View style={styles.content}>
              <Text>Hello</Text>
              <Button onClick={handleClick} />
            </View>
            <Separator />
          </View>
        );
      }
    `;
    const output = await prettierV3.format(code, getOptions());
    // Should not have empty lines after opening tags or before closing tags
    expect(output).toMatchInlineSnapshot(`
      "function Foo() {
        return (
          <View style={styles.root}>
            <View style={styles.content}>
              <Text>Hello</Text>
              <Button onClick={handleClick} />
            </View>
            <Separator />
          </View>
        );
      }
      "
    `);
  });
});
