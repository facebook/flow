/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import {
  printForSnapshotESTree,
  parseForSnapshotESTree,
  printForSnapshotBabel,
  parseForSnapshotBabel,
} from '../__test_utils__/parse';

describe('ComponentDeclaration', () => {
  describe('Basic', () => {
    const code = `
      component Foo() {}
    `;

    test('ESTree', async () => {
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(
        `"function Foo(): React.Node {}"`,
      );
    });
  });

  describe('Complex params', () => {
    const code = `
      component Foo(bar: Bar, baz as boo?: Baz, 'data-bav' as bav: Bav) {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
        "function Foo({
          bar,
          baz: boo,
          'data-bav': bav
        }: $ReadOnly<{
          bar: Bar,
          baz?: Baz,
          'data-bav': Bav,
        }>): React.Node {}"
      `);
    });
  });

  describe('default params', () => {
    const code = `
      component Foo(bar: Bar = '') {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
        "function Foo({
          bar = ''
        }: $ReadOnly<{
          bar?: Bar
        }>): React.Node {}"
      `);
    });
  });

  describe('destructure params', () => {
    const code = `
      component Foo(bar as {baz}: Bar) {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
        "function Foo({
          bar: {
            baz
          }
        }: $ReadOnly<{
          bar: Bar
        }>): React.Node {}"
      `);
    });
  });

  describe('renders type', () => {
    const code = `
      component Foo() renders SpecialType {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(
        `"function Foo(): React.Node {}"`,
      );
    });
  });
  describe('renders maybe type', () => {
    const code = `
      component Foo() renders? SpecialType {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(
        `"function Foo(): React.Node {}"`,
      );
    });
  });

  describe('renders star type', () => {
    const code = `
      component Foo() renders* SpecialType {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(
        `"function Foo(): React.Node {}"`,
      );
    });
  });

  describe('renders type (complex)', () => {
    const code = `
      component Foo() renders (SpecialType | OtherSpecialType) {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(
        `"function Foo(): React.Node {}"`,
      );
    });
  });

  describe('type parameters', () => {
    const code = `
      component Foo<T1, T2>(bar: T1) renders T2 {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
        "function Foo<T1, T2>({
          bar
        }: $ReadOnly<{
          bar: T1
        }>): React.Node {}"
      `);
    });
  });

  describe('rest params', () => {
    const code = `
component Foo(...props: Props) {}
component Foo(...{prop}: Props) {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
        "function Foo(props: Props): React.Node {}

        function Foo({
          prop
        }: Props): React.Node {}"
      `);
    });
  });

  describe('normal and rest params', () => {
    const code = `
      component Foo(param1: string, ...{param2}: Props) {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
        "function Foo({
          param1,
          param2
        }: $ReadOnly<{ ...Props,
          param1: string,
        }>): React.Node {}"
      `);
    });
  });

  describe('normal and rest params with nested rest', () => {
    const code = `
      component Foo(param1: string, ...{param2, ...otherParams}: Props) {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
        "function Foo({
          param1,
          param2,
          ...otherParams
        }: $ReadOnly<{ ...Props,
          param1: string,
        }>): React.Node {}"
      `);
    });
  });

  describe('ref param', () => {
    const code = `
      component Foo(ref: Ref) {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
        "const Foo = React.forwardRef(Foo_withRef);

        function Foo_withRef(_$$empty_props_placeholder$$: $ReadOnly<{}>, ref: Ref): React.Node {}"
      `);
    });
  });

  describe('ref param renamed', () => {
    const code = `
      component Foo(ref as internalRef: Ref) {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
        "const Foo = React.forwardRef(Foo_withRef);

        function Foo_withRef(_$$empty_props_placeholder$$: $ReadOnly<{}>, internalRef: Ref): React.Node {}"
      `);
    });
  });

  describe('ref param renamed destructure', () => {
    const code = `
      component Foo(ref as {current}: Ref) {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
        "const Foo = React.forwardRef(Foo_withRef);

        function Foo_withRef(_$$empty_props_placeholder$$: $ReadOnly<{}>, {
          current
        }: Ref): React.Node {}"
      `);
    });
  });

  describe('ref param with default', () => {
    const code = `
      component Foo(ref: Ref = {}) {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
        "const Foo = React.forwardRef(Foo_withRef);

        function Foo_withRef(_$$empty_props_placeholder$$: $ReadOnly<{}>, ref: Ref = {}): React.Node {}"
      `);
    });
  });

  describe('ref and normal params', () => {
    const code = `
      component Foo(foo: string, ref: Ref) {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
        "const Foo = React.forwardRef(Foo_withRef);

        function Foo_withRef({
          foo
        }: $ReadOnly<{
          foo: string
        }>, ref: Ref): React.Node {}"
      `);
    });
  });

  describe('ref and normal params default exported', () => {
    const code = `
      export default component Foo(foo: string, ref: Ref) {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
        "const Foo = React.forwardRef(Foo_withRef);

        function Foo_withRef({
          foo
        }: $ReadOnly<{
          foo: string
        }>, ref: Ref): React.Node {}

        export default Foo;"
      `);
    });
  });

  describe('ref and normal params named exported', () => {
    const code = `
      export component Foo(foo: string, ref: Ref) {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
        "const Foo = React.forwardRef(Foo_withRef);

        function Foo_withRef({
          foo
        }: $ReadOnly<{
          foo: string
        }>, ref: Ref): React.Node {}

        export { Foo };"
      `);
    });
  });

  describe('ref and normal params within block', () => {
    const code = `
function A() {
  component Foo(foo: string, ref: Ref) {}
  return Foo;
}
if (true) {
  component Foo(foo: string, ref: Ref) {}
  callSomething(Foo);
}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
        "function A() {
          const Foo = React.forwardRef(Foo_withRef);

          function Foo_withRef({
            foo
          }: $ReadOnly<{
            foo: string
          }>, ref: Ref): React.Node {}

          return Foo;
        }

        if (true) {
          const Foo = React.forwardRef(Foo_withRef);

          function Foo_withRef({
            foo
          }: $ReadOnly<{
            foo: string
          }>, ref: Ref): React.Node {}

          callSomething(Foo);
        }"
      `);
    });
  });

  describe('ref and normal params within case', () => {
    const code = `
switch (thing) {
  case 1:
    component Foo(foo: string, ref: Ref) {}
    callSomething(Foo);
}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
        "switch (thing) {
          case 1:
            const Foo = React.forwardRef(Foo_withRef);

            function Foo_withRef({
              foo
            }: $ReadOnly<{
              foo: string
            }>, ref: Ref): React.Node {}

            callSomething(Foo);
        }"
      `);
      expect(await printForSnapshotBabel(code, {reactRuntimeTarget: '19'}))
        .toMatchInlineSnapshot(`
        "switch (thing) {
          case 1:
            function Foo({
              foo,
              ref
            }: $ReadOnly<{
              foo: string,
              ref: Ref,
            }>): React.Node {}

            callSomething(Foo);
        }"
      `);
    });
  });

  describe('ref and normal params with hoisting', () => {
    const code = `
Bar;
unrelated;
someSideEffect(Foo);
unrelated;

component Foo(foo: string, ref: Ref) {}

Bar;
component Bar(foo: string, ref: Ref) {}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
        "const Bar = React.forwardRef(Bar_withRef);
        Bar;
        unrelated;
        const Foo = React.forwardRef(Foo_withRef);
        someSideEffect(Foo);
        unrelated;

        function Foo_withRef({
          foo
        }: $ReadOnly<{
          foo: string
        }>, ref: Ref): React.Node {}

        Bar;

        function Bar_withRef({
          foo
        }: $ReadOnly<{
          foo: string
        }>, ref: Ref): React.Node {}"
      `);
    });
  });

  describe('ref and normal params with hoisting (recursive)', () => {
    const code = `
component Foo(bar: mixed = Foo, ref: any) {
  return null;
}
    `;

    test('ESTree', async () => {
      expect(await printForSnapshotESTree(code)).toBe(code.trim());
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(`
        "const Foo = React.forwardRef(Foo_withRef);

        function Foo_withRef({
          bar = Foo
        }: $ReadOnly<{
          bar?: mixed
        }>, ref: any): React.Node {
          return null;
        }"
      `);
    });
  });

  describe('async', () => {
    const code = `
      async component Foo() {}
    `;

    test('ESTree', async () => {
      expect(await parseForSnapshotESTree(code)).toMatchSnapshot();
      // TODO: Enable print round-trip test once prettier fork supports
      // async component syntax.
      // expect(await printForSnapshotESTree(code)).toBe(code.trim());
    });

    test('Babel', async () => {
      expect(await parseForSnapshotBabel(code)).toMatchSnapshot();
      expect(await printForSnapshotBabel(code)).toMatchInlineSnapshot(
        `"async function Foo(): React.Node {}"`,
      );
    });
  });
});
