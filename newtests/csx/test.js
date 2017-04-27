/*
 * @flow
 * @lint-ignore-every LINE_WRAP1
 */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('@csx and @jsx pragma are mutually exclusive', [
    addCode(`
      // @jsx Foo
      // @csx
    `)
      .newErrors(
        `
          test.js:5
            5: // @csx
                  ^^^^ Unexpected @jsx declaration. Only one per file is allowed.
        `,
      ),
  ]),
  test('Should raise an error if JSX references an identifier not in scope', [
    addCode(`
      // @csx
      <Bar x={23} />;
    `)
    .newErrors(
      `
        test.js:5
          5:       <Bar x={23} />;
                   ^^^^^^^^^^^^^^ identifier \`Bar\`. Could not resolve name
      `,
    ),
  ]),
  test('Should raise no errors if JSX references a function with correct types', [
    addCode(`
      // @csx
      type Props = {|x: number|};
      function Bar(props: Props) {}
      <Bar x={23} />;
      <Bar x={23}></Bar>;
    `)
    .noNewErrors(),
  ]),
  test('Should raise no errors if JSX uses a string literal attribute value', [
    addCode(`
      // @csx
      type Props = {|x: string|};
      function Bar(props: Props) {}
      <Bar x="23" />;
    `)
    .noNewErrors(),
  ]),
  test('Should raise an error referencing JSX element if types mismatch', [
    addCode(`
      // @csx
      type Props = {|x: string|};
      function Bar(props: Props) {}
      <Bar x={23} />;
    `)
    .newErrors(
      `
        test.js:7
          7:       <Bar x={23} />;
                   ^^^^^^^^^^^^^^ props of JSX element \`Bar\`. This type is incompatible with
          6:       function Bar(props: Props) {}
                                       ^^^^^ object type
          Property \`x\` is incompatible:
              7:       <Bar x={23} />;
                               ^^ number. This type is incompatible with
              5:       type Props = {|x: string|};
                                         ^^^^^^ string
      `
    ),
  ]),
  test('Should raise no errors if a JSX spread provides all required attributes for an inexact type', [
    addCode(`
      // @csx
      type Props = {x: string};
      function Bar(props: Props) {}
      const params = {x: '23'};
      <Bar {...params} />;
    `)
    .noNewErrors(),
  ]),
  test('Should raise no errors if a JSX spread provides all required attributes for an exact type', [
    addCode(`
      // @csx
      type Props = {|x: string|};
      function Bar(props: Props) {}
      const params = {x: '23'};
      <Bar {...params} />;
    `)
    .newErrors(  // TODO should not raise any errors.
      `
        test.js:8      8:       <Bar {...params} />;
                   ^^^^^^^^^^^^^^^^^^^ JSX desugared to \`Bar(...)\`
          8:       <Bar {...params} />;
                   ^^^^^^^^^^^^^^^^^^^ props of JSX element \`Bar\`. Inexact type is incompatible with exact type
          6:       function Bar(props: Props) {}
                                       ^^^^^ exact type: object type
      `
    ),
  ]),
  test('Should raise no errors if two separate JSX spreads together provide all required attributes', [
    addCode(`
      // @csx
      type Props = {x: string, y: number};
      function Bar(props: Props) {}
      const params1 = {x: '23'};
      const params2 = {y: 23};
      <Bar {...params1} {...params2} />;
    `)
    .newErrors( // TODO should not raise an error; Flow incorrectly only recognizes the *last* spread.
      `
        test.js:9
          9:       <Bar {...params1} {...params2} />;
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ JSX desugared to \`Bar(...)\`
          6:       function Bar(props: Props) {}
                                       ^^^^^ property \`x\`. Property not found in
          9:       <Bar {...params1} {...params2} />;
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ props of JSX element \`Bar\`
      `
    ),
  ]),
  test('Should raise an error if JSX attributes from a spread do not match parameters', [
    addCode(`
      // @csx
      type Props = {|x: string|};
      function Bar(props: Props) {}
      const params = {x: 23};
      <Bar {...params} />;
    `)
    .newErrors( // TODO this should fail with an error, but a different one.
      `
        test.js:8
          8:       <Bar {...params} />;
                   ^^^^^^^^^^^^^^^^^^^ JSX desugared to \`Bar(...)\`
          8:       <Bar {...params} />;
                   ^^^^^^^^^^^^^^^^^^^ props of JSX element \`Bar\`. Inexact type is incompatible with exact type
          6:       function Bar(props: Props) {}
                                       ^^^^^ exact type: object type
      `
    ),
  ]),
  test('Should raise no errors for a JSX element with a valid member expression', [
    addCode(`
      // @csx
      type Props = {|x: number|};
      const Foo = {
        Bar: (props: Props) => {},
      };
      <Foo.Bar x={23} />;
    `)
    .noNewErrors(),
  ]),
  test('Should raise an error for JSX with a valid member expression but invalid props', [
    addCode(`
      // @csx
      type Props = {|x: string|};
      const Foo = {
        Bar: (props: Props) => {},
      };
      <Foo.Bar x={23} />;
    `)
    .noNewErrors(), // Should fail but does not.
  ]),
  test('Should raise an error for an invalid JSX member expression', [
    addCode(`
      // @csx
      type Props = {|x: number|};
      const Foo = {
        Bar: (props: Props) => {},
      };
      <Foo.WRONG_KEY x={23} />;
    `)
    .noNewErrors(), // Should fail but does not.
  ]),
  test('Should pass text children as a prop named children', [
    addCode(`
      // @csx
      type Props = {|children: ['Test']|};
      function Bar(props: Props) {}
      <Bar>Test</Bar>
    `)
    .noNewErrors(),
  ]),
  test('Should raise an error if you pass children to a function with no children prop', [
    addCode(`
      // @csx
      type Props = {||};
      function Bar(props: Props) {}
      <Bar>Test</Bar>
    `)
    .newErrors(
      `
        test.js:7
          7:       <Bar>Test</Bar>
                   ^^^^^ JSX desugared to \`Bar(...)\`
          7:       <Bar>Test</Bar>
                   ^^^^^ property \`children\`. Property not found in
          6:       function Bar(props: Props) {}
                                       ^^^^^ object type
      `
    ),
  ]),
  test('Should raise an error if you pass a JSX child that returns an invalid type', [
    addCode(`
      // @csx
      type FooProps = {|children: Array<string>|};
      function Foo(props: FooProps) {}
      type BarProps = {||};
      function Bar(props: BarProps): number {return 0;}
      <Foo><Bar /></Foo>
    `)
    .newErrors(
      `
        test.js:9
          9:       <Foo><Bar /></Foo>
                   ^^^^^ JSX desugared to \`Foo(...)\`
          9:       <Foo><Bar /></Foo>
                        ^^^^^^^ number. This type is incompatible with
          5:       type FooProps = {|children: Array<string>|};
                                                     ^^^^^^ string
      `
    ),
  ]),
  test('Should strip whitespace from multiline strings', [
    addCode(`
      // @csx
      type Props = {|children: ['Facebook has many products']|};
      function Bar(props: Props) {}
      <Bar>
        Facebook
             has
        many
          products
      </Bar>
    `)
    .noNewErrors(),
  ]),
  test('Should correctly handle JSX expression children', [
    addCode(`
      // @csx
      type Props = {|children: ['A', 'B']|};
      function Bar(props: Props) {}
      <Bar>{'A'}{'B'}</Bar>
    `)
    .noNewErrors(),
  ]),
  test('Should raise an error if JSX expression children have an incorrect type', [
    addCode(`
      // @csx
      type Props = {|children: ['A']|};
      function Bar(props: Props) {}
      <Bar>{42}</Bar>
    `)
    .newErrors(
      `
        test.js:7
          7:       <Bar>{42}</Bar>
                   ^^^^^ JSX desugared to \`Bar(...)\`
          5:       type Props = {|children: ['A']|};
                                             ^^^ string literal \`A\`. This type is incompatible with
          7:       <Bar>{42}</Bar>
                         ^^ number

        test.js:7
          7:       <Bar>{42}</Bar>
                   ^^^^^ JSX desugared to \`Bar(...)\`
          7:       <Bar>{42}</Bar>
                         ^^ number. This type is incompatible with
          5:       type Props = {|children: ['A']|};
                                             ^^^ string literal \`A\`
      `
    ),
  ]),
]);
