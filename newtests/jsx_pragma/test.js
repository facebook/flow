/*
 * @flow
 * @lint-ignore-every LINE_WRAP1
 */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('@jsx pragma without expression is disallowed', [
    addCode('// @jsx')
      .newErrors(
        `
          test.js:3
            3: // @jsx
                  ^^^^ Invalid @jsx declaration. Should have form \`@jsx LeftHandSideExpression\` with no spaces.
        `,
      ),
  ]),
  test('@jsx pragma with a non-left-hand-side expression is disallowed', [
    addCode('// @jsx (x)=>x')
      .newErrors(
        `
          test.js:3
            3: // @jsx (x)=>x
                       ^^^^^^ Invalid @jsx declaration. Should have form \`@jsx LeftHandSideExpression\` with no spaces. Parse error: Unexpected token =>
        `,
      ),
  ]),
  test('@jsx pragma with a newline should have the right error location', [
    addCode(`
      /* @jsx
           (x)=>x
       */
    `)
      .newErrors(
        `
          test.js:5
            5:            (x)=>x
                          ^^^^^^ Invalid @jsx declaration. Should have form \`@jsx LeftHandSideExpression\` with no spaces. Parse error: Unexpected token =>
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
                               ^^^ number. This type is incompatible with the expected param type of
          6:       function Foo(x: string) {}
                                   ^^^^^^ string

        test.js:12
         12:         <Bar />;
                     ^^^^^^^ JSX desugared to \`Foo(...)\`
          5:       const Bar = 123;
                               ^^^ number. This type is incompatible with the expected param type of
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
                     ^^^^^^^^^^^^^^^ props of JSX element \`Bar\`. This type is incompatible with the expected param type of
            5:       function Foo(elem: number, props: { x: string }) {}
                                                       ^^^^^^^^^^^^^ object type
            Property \`x\` is incompatible:
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
                     ^^^^^^^ null. This type is incompatible with the expected param type of
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
                           ^^^^ boolean. This type is incompatible with the expected param type of
            5:       function Foo(elem: number, props: null, child1: number, child2: string) {}
                                                                     ^^^^^^ number

          test.js:8
            8:       <Bar>{true}{/regex/}</Bar>
                     ^^^^^ JSX desugared to \`Foo(...)\`
            8:       <Bar>{true}{/regex/}</Bar>
                                 ^^^^^^^ RegExp. This type is incompatible with the expected param type of
            5:       function Foo(elem: number, props: null, child1: number, child2: string) {}
                                                                                     ^^^^^^ string
        `,
      ),
  ]).flowConfig("_flowconfig_with_flowlib"),
  test('React ignores certain props, but @jsx shouldnt', [
    addCode(`
      // @jsx Foo
      function Foo(elem: number, props: {key: boolean, ref: number}) {}
      const Bar = 123;
      <Bar key="hi" ref="bye" />;
    `)
      .newErrors(
        `
          test.js:7
            7:       <Bar key="hi" ref="bye" />;
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^ props of JSX element \`Bar\`. This type is incompatible with the expected param type of
            5:       function Foo(elem: number, props: {key: boolean, ref: number}) {}
                                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^ object type
            Property \`key\` is incompatible:
                7:       <Bar key="hi" ref="bye" />;
                                  ^^^^ string. This type is incompatible with
                5:       function Foo(elem: number, props: {key: boolean, ref: number}) {}
                                                                 ^^^^^^^ boolean

          test.js:7
            7:       <Bar key="hi" ref="bye" />;
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^ props of JSX element \`Bar\`. This type is incompatible with the expected param type of
            5:       function Foo(elem: number, props: {key: boolean, ref: number}) {}
                                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^ object type
            Property \`ref\` is incompatible:
                7:       <Bar key="hi" ref="bye" />;
                                           ^^^^^ string. This type is incompatible with
                5:       function Foo(elem: number, props: {key: boolean, ref: number}) {}
                                                                               ^^^^^^ number
        `,
      ),
  ]),
  test('jsx intrinsics should pass through a string', [
    addCode(`
      // @jsx Foo
      function Foo(elem: "bar") {}

      <baz />;
    `)
      .newErrors(
        `
          test.js:7
            7:       <baz />;
                     ^^^^^^^ JSX desugared to \`Foo(...)\`
            7:       <baz />;
                     ^^^^^^^ JSX Intrinsic: \`baz\`. Expected string literal \`bar\`, got \`baz\` instead
            5:       function Foo(elem: "bar") {}
                                        ^^^^^ string literal \`bar\`
        `,
      ),
  ]).flowConfig("_flowconfig_with_flowlib"),
  test('JSX element missing property should error', [
    addCode(`
      // @jsx Foo
      function Foo(elem: number, props: {x: string}) {}
      const Bar = 123;

      <Bar y="hi" />;
    `)
      .newErrors(
        `
          test.js:8
            8:       <Bar y="hi" />;
                     ^^^^^^^^^^^^^^ JSX desugared to \`Foo(...)\`
            8:       <Bar y="hi" />;
                     ^^^^^^^^^^^^^^ props of JSX element \`Bar\`. This type is incompatible with the expected param type of
            5:       function Foo(elem: number, props: {x: string}) {}
                                                       ^^^^^^^^^^^ object type
            Property \`x\` is incompatible:
                5:       function Foo(elem: number, props: {x: string}) {}
                                                           ^^^^^^^^^^^ property \`x\`. Property not found in
                8:       <Bar y="hi" />;
                         ^^^^^^^^^^^^^^ props of JSX element \`Bar\`
        `,
      ),
  ]),
  test('Missing JSX element', [
    addCode(`
      // @jsx Foo
      function Foo(elem: number) {}

      <Bar y="hi" />;
    `)
      .newErrors(
        `
          test.js:7
            7:       <Bar y="hi" />;
                     ^^^^^^^^^^^^^^ identifier \`Bar\`. Could not resolve name
        `,
      ),
  ]),
  test('Exact prop type without spread should work', [
    addCode(`
      // @jsx Foo
      function Foo(elem: number, props: {| x: string |}) {}
      const Bar = 123;

      <Bar x="hi" />;
    `).noNewErrors(),
  ]),
  test('Exact prop type with spread still doesnt work', [
    addCode(`
      // @jsx Foo
      function Foo(elem: number, props: {| x: string |}) {}
      const Bar = 123;

      const props = {x: "hi"};
      <Bar {...props} />;
    `).newErrors(
        `
          test.js:9
            9:       <Bar {...props} />;
                     ^^^^^^^^^^^^^^^^^^ JSX desugared to \`Foo(...)\`
            9:       <Bar {...props} />;
                     ^^^^^^^^^^^^^^^^^^ props of JSX element \`Bar\`. Inexact type is incompatible with exact type
            5:       function Foo(elem: number, props: {| x: string |}) {}
                                                       ^^^^^^^^^^^^^^^ exact type: object type
        `,
      ),
  ]),
  test('Whitespace trimming', [
    addCode(`
      // @jsx Foo
      function Foo(
        elem: number,
        props: null,
        child1: 'hello',
        child2: boolean,
        child3: 'bye',
        ...rest: Array<void>
      ) {}
      const Bar = 123;
      <Bar>

        hi
        {true}
        bye
        there

      </Bar>;
    `).newErrors(
        `
          test.js:14
           14:       <Bar>
                     ^^^^^ JSX desugared to \`Foo(...)\`
           16:         hi
                       ^^ JSX text. Expected string literal \`hello\`, got \`hi\` instead
            8:         child1: 'hello',
                               ^^^^^^^ string literal \`hello\`

          test.js:14
           14:       <Bar>
                     ^^^^^ JSX desugared to \`Foo(...)\`
           18:         bye
                       ^ JSX text. Expected string literal \`bye\`, got \`bye there\` instead
           10:         child3: 'bye',
                               ^^^^^ string literal \`bye\`
        `,
      ),
  ]),
  test('Empty JSXText children are stripped out', [
    addCode(`
      // @jsx Foo
      function Foo(
        elem: number,
        props: null,
        child1: "should be single space",
        child2: "should be true",
        child3: "should be empty string",
        child4: "should be single space",
        ...rest: Array<void>
      ) {}
      const Bar = 123;

      <Bar> {true}
      {''} </Bar>;
    `)
      .newErrors(
        `
          test.js:16
           16:       <Bar> {true}
                     ^^^^^ JSX desugared to \`Foo(...)\`
           16:       <Bar> {true}
                          ^ JSX text. Expected string literal \`should be single space\`, got \` \` instead
            8:         child1: "should be single space",
                               ^^^^^^^^^^^^^^^^^^^^^^^^ string literal \`should be single space\`

          test.js:16
           16:       <Bar> {true}
                     ^^^^^ JSX desugared to \`Foo(...)\`
           16:       <Bar> {true}
                            ^^^^ boolean. This type is incompatible with the expected param type of
            9:         child2: "should be true",
                               ^^^^^^^^^^^^^^^^ string literal \`should be true\`

          test.js:16
           16:       <Bar> {true}
                     ^^^^^ JSX desugared to \`Foo(...)\`
           17:       {''} </Bar>;
                      ^^ string. Expected string literal \`should be empty string\`, got \`\` instead
           10:         child3: "should be empty string",
                               ^^^^^^^^^^^^^^^^^^^^^^^^ string literal \`should be empty string\`

          test.js:16
           16:       <Bar> {true}
                     ^^^^^ JSX desugared to \`Foo(...)\`
           17:       {''} </Bar>;
                         ^ JSX text. Expected string literal \`should be single space\`, got \` \` instead
           11:         child4: "should be single space",
                               ^^^^^^^^^^^^^^^^^^^^^^^^ string literal \`should be single space\`
        `,
      )
      .because('JSXText children with only whitespace or newlines are ignored'),
  ]),
  test('JSXText trimming', [
    addCode("// @jsx Foo"),
    addCode("const Bar = 123;"),
    addCode(`
      let Foo = (elem: any, props: any, c1: "First Middle Last") => {};
      (<Bar>    First${"     "}
           Middle${"     "}
                Last     </Bar>);
    `)
      .newErrors(
        `
          test.js:9
            9:       (<Bar>    First
                      ^^^^^ JSX desugared to \`Foo(...)\`
            9:       (<Bar>    First
                           ^ JSX text. Expected string literal \`First Middle Last\`, got \`    First Middle Last     \` instead
            8:       let Foo = (elem: any, props: any, c1: "First Middle Last") => {};
                                                           ^^^^^^^^^^^^^^^^^^^ string literal \`First Middle Last\`
        `,
      )
      .because(
        "Leading whitespace on the first line and trailing whiteline on the "+
        "last line is not trimmed",
      ),

    addCode(`
      (<Bar>First

        Middle

      Last</Bar>);
    `)
      .noNewErrors()
      .because('Empty lines are filtered out'),

    addCode("(<Bar>First\tMiddle\tLast</Bar>);")
      .noNewErrors()
      .because("Tabs are turned into spaces"),

    addCode("(<Bar>First    Middle\t \t Last</Bar>)")
      .newErrors(
        `
          test.js:24
           24: (<Bar>First    Middle    Last</Bar>)
                ^^^^^ JSX desugared to \`Foo(...)\`
           24: (<Bar>First    Middle    Last</Bar>)
                     ^^^^^^^^^^^^^^^^^^^^^^^ JSX text. Expected string literal \`First Middle Last\`, got \`First    Middle    Last\` instead
            8:       let Foo = (elem: any, props: any, c1: "First Middle Last") => {};
                                                           ^^^^^^^^^^^^^^^^^^^ string literal \`First Middle Last\`
        `,
      )
      .because("Multiple spaces midline stay as multiple spaces"),
  ]),
]);
