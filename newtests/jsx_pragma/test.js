/*
 * @flow
 */


import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('@jsx pragma without expression is disallowed', [
    addCode('// @jsx')
      .newErrors(
        `
          test.js:3
            3: // @jsx
                  ^^^^ Invalid \`@jsx\` declaration. Should have the form \`@jsx LeftHandSideExpression\` with no spaces. [invalid-jsx-decl]
        `,
      ),
  ]),
  test('@jsx pragma with a non-left-hand-side expression is disallowed', [
    addCode('// @jsx (x)=>x')
      .newErrors(
        `
          test.js:3
            3: // @jsx (x)=>x
                       ^^^^^^ Invalid \`@jsx\` declaration. Should have the form \`@jsx LeftHandSideExpression\` with no spaces. Parse error: Unexpected token \`=>\`, expected the end of input. [invalid-jsx-decl]
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
                          ^^^^^^ Invalid \`@jsx\` declaration. Should have the form \`@jsx LeftHandSideExpression\` with no spaces. Parse error: Unexpected token \`=>\`, expected the end of input. [invalid-jsx-decl]
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
                             ^^^ Cannot resolve name \`Foo\`. [cannot-resolve-name]
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
                             ^^^ Cannot resolve name \`Foo\`. [cannot-resolve-name]
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
                     ^^^^^^^ Cannot resolve name \`Foo\`. [cannot-resolve-name]
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
                     ^^^^^^^ Cannot resolve name \`Foo\`. [cannot-resolve-name]
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
                    ^^^ Cannot create \`Bar\` element because number [1] is incompatible with string [2]. [incompatible-type]
          References:
            5:       const Bar = 123;
                                 ^^^ [1]
            6:       function Foo(x: string) {}
                                     ^^^^^^ [2]

        test.js:12
         12:         <Bar />;
                      ^^^ Cannot create \`Bar\` element because number [1] is incompatible with boolean [2]. [incompatible-type]
          References:
            5:       const Bar = 123;
                                 ^^^ [1]
           11:         const Foo = (y: boolean) => {};
                                       ^^^^^^^ [2]
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
                             ^^^ Cannot create \`Bar\` element because number [1] is incompatible with string [2] in property \`x\`. [incompatible-type]
            References:
              8:       <Bar x={123} />;
                               ^^^ [1]
              5:       function Foo(elem: number, props: { x: string }) {}
                                                              ^^^^^^ [2]
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
                      ^^^ Cannot create \`Bar\` element because null [1] is incompatible with object type [2]. [incompatible-type]
            References:
              8:       <Bar />;
                       ^^^^^^^ [1]
              5:       function Foo(elem: number, props: { x: string }) {}
                                                         ^^^^^^^^^^^^^ [2]
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
                           ^^^^ Cannot create \`Bar\` element because boolean [1] is incompatible with number [2]. [incompatible-type]
            References:
              8:       <Bar>{true}{/regex/}</Bar>
                             ^^^^ [1]
              5:       function Foo(elem: number, props: null, child1: number, child2: string) {}
                                                                       ^^^^^^ [2]

          test.js:8
            8:       <Bar>{true}{/regex/}</Bar>
                                 ^^^^^^^ Cannot create \`Bar\` element because \`RegExp\` [1] is incompatible with string [2]. [incompatible-type]
            References:
              8:       <Bar>{true}{/regex/}</Bar>
                                   ^^^^^^^ [1]
              5:       function Foo(elem: number, props: null, child1: number, child2: string) {}
                                                                                       ^^^^^^ [2]
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
                              ^^^^ Cannot create \`Bar\` element because string [1] is incompatible with boolean [2] in property \`key\`. [incompatible-type]
            References:
              7:       <Bar key="hi" ref="bye" />;
                                ^^^^ [1]
              5:       function Foo(elem: number, props: {key: boolean, ref: number}) {}
                                                               ^^^^^^^ [2]

          test.js:7
            7:       <Bar key="hi" ref="bye" />;
                                       ^^^^^ Cannot create \`Bar\` element because string [1] is incompatible with number [2] in property \`ref\`. [incompatible-type]
            References:
              7:       <Bar key="hi" ref="bye" />;
                                         ^^^^^ [1]
              5:       function Foo(elem: number, props: {key: boolean, ref: number}) {}
                                                                             ^^^^^^ [2]
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
                      ^^^ Cannot create \`baz\` element because \`baz\` [1] is incompatible with string literal \`bar\` [2]. [incompatible-type]
            References:
              7:       <baz />;
                        ^^^ [1]
              5:       function Foo(elem: "bar") {}
                                          ^^^^^ [2]
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
                      ^^^ Cannot create \`Bar\` element because property \`x\` is missing in props [1] but exists in object type [2]. [prop-missing]
            References:
              8:       <Bar y="hi" />;
                       ^^^^^^^^^^^^^^ [1]
              5:       function Foo(elem: number, props: {x: string}) {}
                                                         ^^^^^^^^^^^ [2]
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
                      ^^^ Cannot resolve name \`Bar\`. [cannot-resolve-name]
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
  test('Spread syntax in children should work', [
    addCode(`
      // @jsx Foo
      function Foo(elem: number, props: null, child1: 'a', child2: 'b', child3: 'c') {}

      const Bar = 123;
      <Bar>{...["a", "b", "c"]}</Bar>;
    `).noNewErrors(),
  ]),
  test('Exact prop type with spread should work', [
    addCode(`
      // @jsx Foo
      function Foo(elem: number, props: {| x: string |}) {}
      const Bar = 123;

      const props = {x: "hi"};
      <Bar {...props} />;
    `).noNewErrors(),
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
          test.js:16
           16:         hi
                       ^^ Cannot create \`Bar\` element because JSX text [1] is incompatible with string literal \`hello\` [2]. [incompatible-type]
            References:
             16:         hi
                         ^^ [1]
              8:         child1: 'hello',
                                 ^^^^^^^ [2]

          test.js:18
           18:         bye
                       ^ Cannot create \`Bar\` element because JSX text [1] is incompatible with string literal \`bye\` [2]. [incompatible-type]
            References:
             18:         bye
                         ^ [1]
             10:         child3: 'bye',
                                 ^^^^^ [2]
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
                          ^ Cannot create \`Bar\` element because JSX text [1] is incompatible with string literal \`should be single space\` [2]. [incompatible-type]
            References:
             16:       <Bar> {true}
                            ^ [1]
              8:         child1: "should be single space",
                                 ^^^^^^^^^^^^^^^^^^^^^^^^ [2]

          test.js:16
           16:       <Bar> {true}
                            ^^^^ Cannot create \`Bar\` element because boolean [1] is incompatible with string literal \`should be true\` [2]. [incompatible-type]
            References:
             16:       <Bar> {true}
                              ^^^^ [1]
              9:         child2: "should be true",
                                 ^^^^^^^^^^^^^^^^ [2]

          test.js:17
           17:       {''} </Bar>;
                      ^^ Cannot create \`Bar\` element because string [1] is incompatible with string literal \`should be empty string\` [2]. [incompatible-type]
            References:
             17:       {''} </Bar>;
                        ^^ [1]
             10:         child3: "should be empty string",
                                 ^^^^^^^^^^^^^^^^^^^^^^^^ [2]

          test.js:17
           17:       {''} </Bar>;
                         ^ Cannot create \`Bar\` element because JSX text [1] is incompatible with string literal \`should be single space\` [2]. [incompatible-type]
            References:
             17:       {''} </Bar>;
                           ^ [1]
             11:         child4: "should be single space",
                                 ^^^^^^^^^^^^^^^^^^^^^^^^ [2]
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
                           ^ Cannot create \`Bar\` element because JSX text [1] is incompatible with string literal \`First Middle Last\` [2]. [incompatible-type]
            References:
              9:       (<Bar>    First
                             ^ [1]
              8:       let Foo = (elem: any, props: any, c1: "First Middle Last") => {};
                                                             ^^^^^^^^^^^^^^^^^^^ [2]
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
                     ^^^^^^^^^^^^^^^^^^^^^^^ Cannot create \`Bar\` element because JSX text [1] is incompatible with string literal \`First Middle Last\` [2]. [incompatible-type]
            References:
             24: (<Bar>First    Middle    Last</Bar>)
                       ^^^^^^^^^^^^^^^^^^^^^^^ [1]
              8:       let Foo = (elem: any, props: any, c1: "First Middle Last") => {};
                                                             ^^^^^^^^^^^^^^^^^^^ [2]
        `,
      )
      .because("Multiple spaces midline stay as multiple spaces"),
  ]),
]);
