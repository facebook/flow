/* @flow */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('@jsx pragma without expression is disallowed', [
    addCode('// @jsx')
      .newErrors(
        `
          test.js:3
            3: // @jsx
               ^^^^^^^ Invalid @jsx declaration. Should have form \`@jsx LeftHandSideExpression\` with no spaces.
        `,
      ),
  ]),
  test('@jsx pragma with a non-left-hand-side expression is disallowed', [
    addCode('// @jsx (x)=>x')
      .newErrors(
        `
          test.js:3
            3: // @jsx (x)=>x
               ^^^^^^^^^^^^^^ Invalid @jsx declaration. Should have form \`@jsx LeftHandSideExpression\` with no spaces. Parse error: Unexpected token =>
        `,
      ),
  ]),
  test('Line comment complex @jsx with unknown identifier points to pragma', [
    addCode(`
      // @jsx Foo['Bar']
      var Bar = 123;
      <Bar />;
    `)
      .newErrors(
        `
          test.js:4
            4:       // @jsx Foo['Bar']
                             ^^^ identifier \`Foo\`. Could not resolve name
        `,
      ),
  ]),
  test('Block comment complex @jsx with unknown identifier points to pragma', [
    addCode(`
      /*
       * @jsx Foo['Bar']
       */
      var Bar = 123;
      <Bar />;
    `)
      .newErrors(
        `
          test.js:5
            5:        * @jsx Foo['Bar']
                             ^^^ identifier \`Foo\`. Could not resolve name
        `,
      ),
  ]),
  test('Simple identifier @jsx with unknown identifier has better location', [
    addCode(`
      // @jsx Foo
      var Bar = 123;
      <Bar />;
    `)
      .newErrors(
        `
          test.js:6
            6:       <Bar />;
                     ^^^^^^^ JSX desugared to \`Foo(...)\`. identifier Foo. Could not resolve name
        `,
      ),
  ]),
  test('Simple member @jsx with unknown identifier has better location', [
    addCode(`
      // @jsx Foo.baz
      var Bar = 123;
      <Bar />;
    `)
      .newErrors(
        `
          test.js:6
            6:       <Bar />;
                     ^^^^^^^ JSX desugared to \`Foo.baz(...)\`. identifier Foo. Could not resolve name
        `,
      ),
  ]),
  test('Should respect local scope', [
    addCode(`
      // @jsx Foo
      const Bar = 123;
      function Foo(x: string) {}

      <Bar />;

      {
        const Foo = (y: boolean) => {};
        <Bar />;
      }
    `)
    .newErrors(
      `
        test.js:8
          8:       <Bar />;
                   ^^^^^^^ JSX desugared to \`Foo(...)\`
          5:       const Bar = 123;
                               ^^^ number. This type is incompatible with
          6:       function Foo(x: string) {}
                                   ^^^^^^ string

        test.js:12
         12:         <Bar />;
                     ^^^^^^^ JSX desugared to \`Foo(...)\`
          5:       const Bar = 123;
                               ^^^ number. This type is incompatible with
         11:         const Foo = (y: boolean) => {};
                                     ^^^^^^^ boolean
      `,
    ),
  ]),
  test('Second arg to jsx function should be props', [
    addCode(`
      // @jsx Foo
      function Foo(elem: number, props: { x: string }) {}
      const Bar = 123;

      <Bar x={123} />;
    `).newErrors(
        `
          test.js:8
            8:       <Bar x={123} />;
                     ^^^^^^^^^^^^^^^ JSX desugared to \`Foo(...)\`
            8:       <Bar x={123} />;
                             ^^^ number. This type is incompatible with
            5:       function Foo(elem: number, props: { x: string }) {}
                                                            ^^^^^^ string
        `,
      ),
  ]),
  test('Second arg to jsx function is null when there are no attributes', [
    addCode(`
      // @jsx Foo
      function Foo(elem: number, props: { x: string }) {}
      const Bar = 123;

      <Bar />;
    `).newErrors(
        `
          test.js:8
            8:       <Bar />;
                     ^^^^^^^ JSX desugared to \`Foo(...)\`
            8:       <Bar />;
                     ^^^^^^^ null. This type is incompatible with
            5:       function Foo(elem: number, props: { x: string }) {}
                                                       ^^^^^^^^^^^^^ object type
        `,
      ),
  ]),
  test('Children are passed after the element and props', [
    addCode(`
      // @jsx Foo
      function Foo(elem: number, props: null, child1: number, child2: string) {}
      const Bar = 123;

      <Bar>{true}{/regex/}</Bar>
    `).newErrors(
        `
          test.js:8
            8:       <Bar>{true}{/regex/}</Bar>
                     ^^^^^ JSX desugared to \`Foo(...)\`
            8:       <Bar>{true}{/regex/}</Bar>
                           ^^^^ boolean. This type is incompatible with
            5:       function Foo(elem: number, props: null, child1: number, child2: string) {}
                                                                     ^^^^^^ number

          test.js:8
            8:       <Bar>{true}{/regex/}</Bar>
                                 ^^^^^^^ RegExp. This type is incompatible with
            5:       function Foo(elem: number, props: null, child1: number, child2: string) {}
                                                                                     ^^^^^^ string
        `,
      ),
  ])
]);
