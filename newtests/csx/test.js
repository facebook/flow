/*
 * @flow
 */


import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('@csx and @jsx pragma are mutually exclusive', [
    addCode(`
      // @jsx Foo
      // @csx
    `)
      .newErrors(
        `
          test.js:5
            5:       // @csx
                        ^^^^ Unexpected \`@jsx\` declaration. Only one per file is allowed.
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
                    ^^^ Cannot resolve name \`Bar\`.
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
                           ^^ Cannot create \`Bar\` element because number [1] is incompatible with string [2] in property \`x\`.
          References:
            7:       <Bar x={23} />;
                             ^^ [1]
            5:       type Props = {|x: string|};
                                       ^^^^^^ [2]
      `,
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
  test('Should raise no errors if CSX children are passed as spread with list inline', [
    addCode(`
      // @csx
      type Props = {|children: Array<string>|};
      function Foo(props: Props) {}
      <Foo>{...["foo", "bar"]}</Foo>;
    `)
    .noNewErrors(),
  ]),
  test('Should raise no errors if CSX children are passed as spread with variable', [
    addCode(`
      // @csx
      type Props = {|children: Array<string>|};
      function Foo(props: Props) {}
      const arr = ["foo", "bar"];
      <Foo>{...arr}</Foo>;
    `)
    .noNewErrors(),
  ]),
  test('Should raise an error if CSX spread children are not a list', [
    addCode(`
      // @csx
      type Props = {|children: Array<number>|};
      function Foo(props: Props) {}
      const x = 42;
      <Foo>{...x}</Foo>;
    `)
    .newErrors(
      `
        test.js:8
          8:       <Foo>{...x}</Foo>;
                            ^ property \`@@iterator\` is missing in number [1] but exists in \`$Iterable\` [2].
          References:
            7:       const x = 42;
                               ^^ [1]
           48: interface $Iterable<+Yield,+Return,-Next> {
                         ^^^^^^^^^ [2]. See lib: [LIB] prelude.js:48
      `,
    )
  ]),
  test('Should raise an error if CSX children passed as spread have the wrong type', [
    addCode(`
      // @csx
      type Props = {|children: Array<number>|};
      function Foo(props: Props) {}
      const arr = ["foo"];
      <Foo>{...arr}</Foo>;
    `)
    .newErrors(
      `
        test.js:8
          8:       <Foo>{...arr}</Foo>;
                    ^^^ Cannot create \`Foo\` element because string [1] is incompatible with number [2] in array element of property \`children\`.
          References:
            7:       const arr = ["foo"];
                                  ^^^^^ [1]
            5:       type Props = {|children: Array<number>|};
                                                    ^^^^^^ [2]
      `,
    ),
  ]),
  test('Should raise no errors if CSX children are passed as spread with function call', [
    addCode(`
      // @csx
      type Props = {|children: Array<number>|};
      type TProps = {|text: string|};
      function Foo(props: Props) {}
      function Title(props: TProps) { return 42; }
      function get_titles(l) {
        return [];
      }
      var arr = ["foo", "bar"];
      <Foo>{...get_titles(arr)}</Foo>;
    `)
    .noNewErrors(),
  ]),
  test('Should raise no errors if CSX children are passed as spread with other children', [
    addCode(`
      // @csx
      type Props = {|children: Array<string>|};
      function Foo(props: Props) {}
      <Foo>
      {"foobar"}
      {...["foo", "bar"]}
      {"baz"}
      </Foo>;
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
    .noNewErrors(),
  ]),
  test('Should raise no errors if two separate JSX spreads together provide all required attributes', [
    addCode(`
      // @csx
      type Props = {x: string, y: number};
      function Bar(props: Props) {}
      const params1 = {x: '23'};
      const params2 = {y: 23};
      <Bar {...params1} {...params2} />;
    `).noNewErrors(),
  ]),
  test('Should raise an error if JSX attributes from a spread do not match parameters', [
    addCode(`
      // @csx
      type Props = {|x: string|};
      function Bar(props: Props) {}
      const params = {x: 23};
      <Bar {...params} />;
    `)
    .newErrors(
      `
        test.js:8
          8:       <Bar {...params} />;
                    ^^^ Cannot create \`Bar\` element because number [1] is incompatible with string [2] in property \`x\`.
          References:
            7:       const params = {x: 23};
                                        ^^ [1]
            5:       type Props = {|x: string|};
                                       ^^^^^^ [2]
      `,
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
                    ^^^ Cannot create \`Bar\` element because property \`children\` is missing in \`Props\` [1] but exists in props [2].
          References:
            6:       function Bar(props: Props) {}
                                         ^^^^^ [1]
            7:       <Bar>Test</Bar>
                     ^^^^^^^^^^^^^^^ [2]
      `,
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
                        ^^^^^^^ Cannot create \`Foo\` element because number [1] is incompatible with string [2] in array element of property \`children\`.
          References:
            8:       function Bar(props: BarProps): number {return 0;}
                                                    ^^^^^^ [1]
            5:       type FooProps = {|children: Array<string>|};
                                                       ^^^^^^ [2]
      `,
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
                         ^^ Cannot create \`Bar\` element because number [1] is incompatible with string literal \`A\` [2] in index 0 of property \`children\`.
          References:
            7:       <Bar>{42}</Bar>
                           ^^ [1]
            5:       type Props = {|children: ['A']|};
                                               ^^^ [2]
      `,
    ),
  ]),
]);
