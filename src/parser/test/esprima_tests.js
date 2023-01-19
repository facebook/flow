module.exports = {
  todo: {
    'ES6: Destructured Parameters': true,
    'Harmony Invalid syntax': true,
  },
  sections: {
    'Function Expression': [
      '(function(){}())',
      'var x = function(){}.bind(this)',
    ],

    'JSX': [
        '<n:a n:v />',
        '<a n:foo="bar"> {value} <b><c /></b></a>',
        '<a b={" "} c=" " d="&amp;" />',
        '<日本語></日本語>',
        '<AbC-def\n  test="&#x0026;&#38;">\nbar\nbaz\n</AbC-def>',
        '<a b={x ? <c /> : <d />} />',
        '<div>@test content</div>',
        '<div><br />7x invalid-js-identifier</div>',
        '<div {...props} />',
        '<div {...props} post="attribute" />',
        '<div pre="leading" pre2="attribute" {...props}></div>',
        '<a>    </a>',
    ],
    'Invalid JSX Syntax': [
        '<a b=d />',
        '<a></b>',
        '<a foo="bar',
        {
          content: '<a:b.c></a:b.c>',
          explanation: "Improved error message",
          expected_differences: {
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Unexpected token .',
              actual: 'Unexpected token `.`, expected the token `>`'
            }
          }
        },
        {
          content: '<a.b:c></a.b:c>',
          explanation: "Improved error message",
          expected_differences: {
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Unexpected token :',
              actual: 'Unexpected token `:`, expected the token `>`'
            }
          }
        },
        '<a.b.c></a>',
        {
          content: '<.a></.a>',
          explanation: "Improved error message",
          expected_differences: {
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Unexpected token .',
              actual: 'Unexpected token `.`, expected the token `>`'
            }
          }
        },
        {
          content: '<a[foo]></a[foo]>',
          explanation: "Improved error message",
          expected_differences: {
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Unexpected token [',
              actual: 'Unexpected token `[`, expected the token `>`'
            }
          }
        },
        {
          content: '<a[\'foo\']></a[\'foo\']>',
          explanation: "Improved error message",
          expected_differences: {
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Unexpected token [',
              actual: 'Unexpected token `[`, expected the token `>`'
            }
          }
        },
        '<a><a />',
        {
          content: '<a>{"str";}</a>',
          explanation: "Improved error message",
          expected_differences: {
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Unexpected token ;',
              actual: 'Unexpected token `;`, expected the token `}`'
            }
          }
        },
        {
          content: '<span className="a", id="b" />',
          explanation: "Improved error message",
          expected_differences: {
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Unexpected token ,',
              actual: 'Unexpected token `,`, expected the token `>`'
            }
          }
        },
        {
          content: '<div className"app">',
          explanation: "Improved error message",
          expected_differences: {
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Unexpected string',
              actual: 'Unexpected string, expected the token `>`'
            }
          }
        },
        {
          content: '<div {props} />',
          explanation: "Improved error message",
          expected_differences: {
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Unexpected identifier',
              actual: 'Unexpected identifier, expected the token `...`'
            }
          }
        },
        {
          content: '<div>stuff</div {...props}>',
          explanation: "Improved error message",
          expected_differences: {
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Unexpected token {',
              actual: 'Unexpected token `{`, expected the token `>`'
            }
          }
        },
        {
          content: '<div {...props}>stuff</div {...props}>',
          explanation: "Improved error message",
          expected_differences: {
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Unexpected token {',
              actual: 'Unexpected token `{`, expected the token `>`'
            }
          }
        },
        '<div><a/><b/><c/>',
    ],

    'ES6 Unicode Code Point Escape Sequence': [
        '"\\u{714E}\\u{8336}"',
        '"\\u{20BB7}\\u{91CE}\\u{5BB6}"'
    ],

    // ECMAScript 6th Syntax, 11.1. 9 Template Literals

    'ES6 Template Strings': [
        '`Hello\rworld`',
        '`foo ${\n  "bar"\n} baz`',
        '( foo)`bar`',
        '[...a.b`hi`.c`bye`]',
        'foo`foo`.bar`bar`.baz',
        'foo`bar``baz`',
        'foo`foo`()',
    ],


    // ECMAScript 6th Syntax, 13.2 Arrow Function Definitions

    'ES6: Arrow Function': [
        '([a, , b]) => 42',
        {
          content: '([a.a]) => 42',
          explanation: 'Flow fails to parse it correctly',
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 7,
              actual: '8-10'
            },
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Invalid left-hand side in formals list',
              actual: 'Unexpected token `=>`, expected the end of an expression statement (`;`)',
            }
          },
        },
        // I don't see why this should be an error. 14.2.9
        {
          content: '(a, a) => 42',
          explanation: "Esprima counts parens in its loc, Flow doesn't",
          expected_differences: {
            'root.errors': {
              type: 'Flow found no error',
              expected: 'Line 1: Strict mode function may not have duplicate parameter names',
              actual: undefined
            },
          }
        },
        '([...a]) => 42',
        '({...a}) => 42',
    ],


    // http://wiki.ecmascript.org/doku.php?id=harmony:destructuring

    'Harmony: Destructuring': [
        '[a, b] = [b, a]',
        '({ responseText: text }) = res',
        'const {a} = {}',
        'const [a] = []',
        'let {a} = {}',
        'let [a] = []',
        'var {a} = {}',
        'var [a] = []',
        'const {a:b} = {}',
        'let {a:b} = {}',
        'var {a:b} = {}',
        'var f = function({node, guestStatus}) {}',
    ],

    // http://wiki.ecmascript.org/doku.php?id=strawman:maximally_minimal_classes

    'Harmony: Class (strawman)': [
        'var A = class extends B {}',
        'class type {}',
        'class A extends class B extends C {} {}',
        {
          content: 'class A {get() {}}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 15,
              actual: 12,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 15,
              actual: 12,
            },
          },
        },
        {
          content: 'class A { static get() {}}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 23,
              actual: 20,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 23,
              actual: 20,
            },
          },
        },
        {
          content: 'class A extends B {get foo() {}}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 29,
              actual: 26,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 29,
              actual: 26,
            },
          },
        },
        {
          content: 'class A extends B { static get foo() {}}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 37,
              actual: 34,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 37,
              actual: 34,
            },
          },
        },
        {
          content: 'class A {set a(v) {}}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 18,
              actual: 14,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 18,
              actual: 14,
            },
          },
        },
        {
          content: 'class A { static set a(v) {}}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 26,
              actual: 22,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 26,
              actual: 22,
            },
          },
        },
        {
          content: 'class A {set(v) {};}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 16,
              actual: 12,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 16,
              actual: 12,
            },
          },
        },
        {
          content: 'class A { static set(v) {};}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 24,
              actual: 20,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 24,
              actual: 20,
            },
          },
        },
        {
          content: 'class A {*gen(v) { yield v; }}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 17,
              actual: 13,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 17,
              actual: 13,
            },
          },
        },
        {
          content: 'class A { static *gen(v) { yield v; }}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 25,
              actual: 21,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 25,
              actual: 21,
            },
          },
        },
        {
          content: '"use strict"; (class A {constructor() { super() }})',
          explanation: "Esprima counts the implicit semicolon in its loc, " +
            "Flow doesn't. Esprima-fb also doesn't include params in " +
            "FunctionExpression location. It also mishandles `super`",
          expected_differences: {
            'root.body.1.expression.body.body.0.value.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 48,
              actual: 47
            },
            'root.body.1.expression.body.body.0.value.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 48,
              actual: 47
            },
            'root.body.1.expression.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 38,
              actual: 35,
            },
            'root.body.1.expression.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 38,
              actual: 35,
            },
            'root.body.1.expression.body.body.0.value.body.body.0.expression.callee.type': {
              type: 'Wrong string',
              expected: 'Identifier',
              actual: 'Super',
            },
            'root.body.1.expression.body.body.0.value.body.body.0.expression.callee.name': {
              type: 'Missing property'
            },
            'root.body.1.expression.body.body.0.value.body.body.0.expression.callee.typeAnnotation': {
              type: 'Missing property'
            },
            'root.body.1.expression.body.body.0.value.body.body.0.expression.callee.optional': {
              type: 'Missing property'
            },
          }
        },
        {
          content: 'class A {static foo() {}}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 22,
              actual: 19,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 22,
              actual: 19,
            },
          },
        },
        {
          content: 'class A {foo() {} static bar() {}}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 15,
              actual: 12,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 15,
              actual: 12,
            },
            'root.body.0.body.body.1.value.range.0': {
              type: 'Wrong number',
              expected: 31,
              actual: 28,
            },
            'root.body.0.body.body.1.value.loc.start.column': {
              type: 'Wrong number',
              expected: 31,
              actual: 28,
            },
          },
        },
        {
          content: 'class A { foo() {} bar() {}}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 16,
              actual: 13,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 16,
              actual: 13,
            },
            'root.body.0.body.body.1.value.range.0': {
              type: 'Wrong number',
              expected: 25,
              actual: 22,
            },
            'root.body.0.body.body.1.value.loc.start.column': {
              type: 'Wrong number',
              expected: 25,
              actual: 22,
            },
          },
        },
        {
          content: 'class A { get foo() {} set foo(v) {}}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 20,
              actual: 17,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 20,
              actual: 17,
            },
            'root.body.0.body.body.1.value.range.0': {
              type: 'Wrong number',
              expected: 34,
              actual: 30,
            },
            'root.body.0.body.body.1.value.loc.start.column': {
              type: 'Wrong number',
              expected: 34,
              actual: 30,
            },
          },
        },
        {
          content: 'class A { static get foo() {} get foo() {}}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 27,
              actual: 24,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 27,
              actual: 24,
            },
            'root.body.0.body.body.1.value.range.0': {
              type: 'Wrong number',
              expected: 40,
              actual: 37,
            },
            'root.body.0.body.body.1.value.loc.start.column': {
              type: 'Wrong number',
              expected: 40,
              actual: 37,
            },
          },
        },
        {
          content: 'class A { static get foo() {} static get bar() {} }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 27,
              actual: 24,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 27,
              actual: 24,
            },
            'root.body.0.body.body.1.value.range.0': {
              type: 'Wrong number',
              expected: 47,
              actual: 44,
            },
            'root.body.0.body.body.1.value.loc.start.column': {
              type: 'Wrong number',
              expected: 47,
              actual: 44,
            },
          },
        },
        {
          content: 'class A { static get foo() {} static set foo(v) {} get foo() {} set foo(v) {}}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 27,
              actual: 24,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 27,
              actual: 24,
            },
            'root.body.0.body.body.1.value.range.0': {
              type: 'Wrong number',
              expected: 48,
              actual: 44,
            },
            'root.body.0.body.body.1.value.loc.start.column': {
              type: 'Wrong number',
              expected: 48,
              actual: 44,
            },
            'root.body.0.body.body.2.value.range.0': {
              type: 'Wrong number',
              expected: 61,
              actual: 58,
            },
            'root.body.0.body.body.2.value.loc.start.column': {
              type: 'Wrong number',
              expected: 61,
              actual: 58,
            },
            'root.body.0.body.body.3.value.range.0': {
              type: 'Wrong number',
              expected: 75,
              actual: 71,
            },
            'root.body.0.body.body.3.value.loc.start.column': {
              type: 'Wrong number',
              expected: 75,
              actual: 71,
            },
          },
        },
        {
          content: 'class A { set foo(v) {} get foo() {} }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 21,
              actual: 17,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 21,
              actual: 17,
            },
            'root.body.0.body.body.1.value.range.0': {
              type: 'Wrong number',
              expected: 34,
              actual: 31,
            },
            'root.body.0.body.body.1.value.loc.start.column': {
              type: 'Wrong number',
              expected: 34,
              actual: 31,
            },
          },
        },
        {
          content: 'class A { [x]() {} }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 16,
              actual: 13,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 16,
              actual: 13,
            },
          },
        },
        {
          content: 'class A { [1+1]() {} }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 18,
              actual: 15,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 18,
              actual: 15,
            },
          },
        },
        {
          content: 'class A { get [1+1]() {} }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 22,
              actual: 19,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 22,
              actual: 19,
            },
          },
        },
        {
          content: 'class A { static get [1+1]() {} }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 29,
              actual: 26,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 29,
              actual: 26,
            },
          },
        },
        {
          content: 'class A { static() {} }',
          explanation: 'Our version of esprima is outdated',
          expected_differences: {
            'root.errors': {
              type: 'Flow found no error',
              expected: 'Line 1: Unexpected token )',
              actual: undefined,
            },
          },
        },
        {
          content: 'class A { static static() {} }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 26,
              actual: 23,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 26,
              actual: 23,
            },
          },
        },
    ],

    'ES6: Computed Properties': [
        '({[x]: 10})',
        '({["x" + "y"]: 10})',
        '({[x]: function() {}})',
        '({[x]: 10, y: 20})',
        {
          content: '({get [x]() {}, set [x](v) {}})',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.expression.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 12,
              actual: 9,
            },
            'root.body.0.expression.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 12,
              actual: 9,
            },
            'root.body.0.expression.properties.1.value.range.0': {
              type: 'Wrong number',
              expected: 27,
              actual: 23,
            },
            'root.body.0.expression.properties.1.value.loc.start.column': {
              type: 'Wrong number',
              expected: 27,
              actual: 23,
            },
          },
        },
        {
          content: '({[x]() {}})',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.expression.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 8,
              actual: 5,
            },
            'root.body.0.expression.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 8,
              actual: 5,
            },
          },
        },
        // These tests fail due to computed Properties
        // 'var {[x]: y} = {y}',
        // 'function f({[x]: y}) {}',
        {
          content: 'var x = {*[test]() { yield *v; }}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.declarations.0.init.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 19,
              actual: 16,
            },
            'root.body.0.declarations.0.init.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 19,
              actual: 16,
            },
          },
        },
    ],

    'ES6: Default parameters': [
        'function f([x] = [1]) {}',
        'function f({x} = {x: 10}) {}',
        'f = function({x} = {x: 10}) {}',
        '({f: function({x} = {x: 10}) {}})',
        {
          content: '({f({x} = {x: 10}) {}})',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.expression.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 19,
              actual: 3,
            },
            'root.body.0.expression.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 19,
              actual: 3,
            },
          },
        },
        {
          content: '(class {f({x} = {x: 10}) {}})',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.expression.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 25,
              actual: 9,
            },
            'root.body.0.expression.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 25,
              actual: 9,
            },
          },
        },
        '(({x} = {x: 10}) => {})',
        {
          content: 'x = { f(a=1) {} }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 13,
              actual: 7,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 13,
              actual: 7,
            },
          },
        },
    ],

    'ES6: Destructured Parameters': [
        'function x([ a, b ]){}',
        'function x({ a, b }){}',
        '"use strict"; function x({ a }){ "use strict"; }',
        'function x(a, { a }){}',
        'function x(...[ a, b ]){}',
        'function x({ a: { w, x }, b: [y, z] }, ...[a, b, c]){}',
        '(function x([ a, b ]){})',
        '(function x({ a, b }){})',
        '(function x(...[ a, b ]){})',
        '(function x({ a: { w, x }, b: [y, z] }, ...[a, b, c]){})',
        '({ x([ a, b ]){} })',
        '({ x(...[ a, b ]){} })',
        '({ x({ a: { w, x }, b: [y, z] }, ...[a, b, c]){} })',
        '(...a) => {}',
        '(a, ...b) => {}',
        '({ a }) => {}',
        '({ a }, ...b) => {}',
        '(...[a, b]) => {}',
        '(a, ...[b]) => {}',
        '({ a: [a, b] }, ...c) => {}',
        '({ a: b, c }, [d, e], ...f) => {}'
    ],

    'ES6: SpreadElement': [
        '[...a] = b',
        '[a, ...b] = c',
        '[{ a, b }, ...c] = d',
        '[a, ...[b, c]] = d',
        'var [...a] = b',
        'var [a, ...b] = c',
        'var [{ a, b }, ...c] = d',
        'var [a, ...[b, c]] = d',
        'func(...a)',
        'func(a, ...b)',
        'new foo(...a)',
        'new foo(a, ...b)',
        'function foo(...a){}',
        'function foo(a, ...b){}',
        {
          content: 'function foo(a, ...a){}',
          explanation: "esprima-fb doesn't catch this",
          expected_differences: {
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: undefined,
              actual: 'Strict mode function may not have duplicate parameter names',
            },
            'root.errors.0.line': {
              type: 'Wrong error line',
              expected: undefined,
              actual: 1,
            },
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: undefined,
              actual: 20,
            },
          }
        }
    ],

    'Harmony Invalid syntax': [
        '"\\u{110000}"',
        '[v] += ary',
        '[2] = 42',
        '({ obj:20 }) = 42',
        '( { get x() {} } ) = 0',
        'x \n is y',
        'x \n isnt y',
        'function default() {}',
        'function hello() {\'use strict\'; ({ i: 10, s(eval) { } }); }',
        'function a() { "use strict"; ({ b(t, t) { } }); }',
        'var super',
        'var default',
        'let default',
        'const default',
        '({ v: eval }) = obj',
        '({ v: arguments }) = obj',
        'for (var i = function() { return 10 in [] } in list) process(x);',
        'for (let x = 42 in list) process(x);',
        'for (let x = 42 of list) process(x);',
        'module\n"crypto" {}',
        'module foo from bar',
        'module 42',
        'module foo bar',
        'module "crypto" { module "e" {} }',
        'module "x" { export * from foo }',
        'import foo',
        'import { foo, bar }',
        'import foo from bar',
        '((a)) => 42',
        '(a, (b)) => 42',
        '"use strict"; (eval = 10) => 42',
        // strict mode, using eval when IsSimpleParameterList is true
        '"use strict"; eval => 42',
        // strict mode, using arguments when IsSimpleParameterList is true
        '"use strict"; arguments => 42',
        // strict mode, using eval when IsSimpleParameterList is true
        '"use strict"; (eval, a) => 42',
        // strict mode, using arguments when IsSimpleParameterList is true
        '"use strict"; (arguments, a) => 42',
        // strict mode, using eval when IsSimpleParameterList is false
        '"use strict"; (eval, a = 10) => 42',
        '(a, a) => 42',
        '"use strict"; (a, a) => 42',
        '"use strict"; (a) => 00',
        '() <= 42',
        '(10) => 00',
        '(10, 20) => 00',
        'yield v',
        'yield 10',
        'yield* 10',
        'e => yield* 10',
        '(function () { yield* 10 })',
        '(function() { "use strict"; f(yield v) })',
        'var obj = { *test** }',
        'class A extends yield B { }',
        'class default',
        '`test',
        'switch `test`',
        '`hello ${10 `test`',
        '`hello ${10;test`',
        'function a() 1 // expression closure is not supported',
        '[a,b if (a)] // (a,b)',
        'for each (let x in {}) {};',
        '[x for (let x in [])]',
        '[x for (const x in [])]',
        '[x for (var x in [])]',
        '[a,b for (a in [])] // (a,b) ',
        '[x if (x)]  // block required',
        'var a = [x if (x)]',
        '[for (x in [])]  // no espression',
        '({ "chance" }) = obj',
        '({ 42 }) = obj',
        'function f(a, ...b = 0)',
        'function x(...{ a }){}',
        '"use strict"; function x(a, { a }){}',
        '"use strict"; function x({ b: { a } }, [{ b: { a } }]){}',
        '"use strict"; function x(a, ...[a]){}',
        '(...a, b) => {}',
        '([ 5 ]) => {}',
        '({ 5 }) => {}',
        '(...[ 5 ]) => {}',
        '[...{ a }] = b',
        '[...a, b] = c',
        'func(...a, b)',
        'let {...a, b} = x',
        '(function({...a, b}) {})',
        '({ t(eval) { "use strict"; } });',
        '"use strict"; `${test}\\02`;',
        '[...a, ] = b',
        'if (b,...a, );',
        '(b, ...a)',
        'module "Universe" { ;  ;  ',
        'switch (cond) { case 10: let a = 20; ',
        '"use strict"; (eval) => 42',
        '(eval) => { "use strict"; 42 }',
        '({ get test() { } }) => 42'
    ],
    'Type Annotations': [
        'a = class Foo<T> { }',
        'a = class Foo<T> extends Bar<T> {}',
        'a=function<T,S>() {}',
        {
          content: 'a={set fooProp(value:number){}}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 28,
              actual: 14,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 28,
              actual: 14,
            },
          },
        },
        {
          content: 'class Array { concat(items:number | string) {}; }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 44,
              actual: 20,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 44,
              actual: 20,
            },
          },
        },
        {
          content: 'class Foo { [1 + 1]: string; }',
          explanation: 'Esprima-fb returns ClassProperty, but Flow returns PropertyDefinition',
          expected_differences: {
            'root.body.0.body.body.0.type': {
              type: 'Wrong string',
              expected: 'ClassProperty',
              actual: 'PropertyDefinition'
            }
          }
        },
        {
          content: 'class Foo { 123:string; }',
          explanation: 'Esprima-fb returns ClassProperty, but Flow returns PropertyDefinition',
          expected_differences: {
            'root.body.0.body.body.0.type': {
              type: 'Wrong string',
              expected: 'ClassProperty',
              actual: 'PropertyDefinition'
            }
          }
        },
        {
          content: 'class Foo { "bar"<T>() { } }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 23,
              actual: 17,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 23,
              actual: 17,
            },
          },
        },
        {
          content: 'class Foo { "prop1":string; }',
          explanation: 'Esprima-fb returns ClassProperty, but Flow returns PropertyDefinition',
          expected_differences: {
            'root.body.0.body.body.0.type': {
              type: 'Wrong string',
              expected: 'ClassProperty',
              actual: 'PropertyDefinition'
            }
          }
        },
        {
          content: 'class Foo { [prop1]: string; }',
          explanation: 'Esprima-fb returns ClassProperty, but Flow returns PropertyDefinition',
          expected_differences: {
            'root.body.0.body.body.0.type': {
              type: 'Wrong string',
              expected: 'ClassProperty',
              actual: 'PropertyDefinition'
            }
          }
        },
        {
          content: 'class Foo { prop1:string; prop2:number; }',
          explanation: 'Esprima-fb returns ClassProperty, but Flow returns PropertyDefinition',
          expected_differences: {
            'root.body.0.body.body.0.type': {
              type: 'Wrong string',
              expected: 'ClassProperty',
              actual: 'PropertyDefinition'
            },
            'root.body.0.body.body.1.type': {
              type: 'Wrong string',
              expected: 'ClassProperty',
              actual: 'PropertyDefinition'
            }
          }
        },
        {
          content: 'class Foo { static prop1:string; prop2:number; }',
          explanation: 'Esprima-fb returns ClassProperty, but Flow returns PropertyDefinition',
          expected_differences: {
            'root.body.0.body.body.0.type': {
              type: 'Wrong string',
              expected: 'ClassProperty',
              actual: 'PropertyDefinition'
            },
            'root.body.0.body.body.1.type': {
              type: 'Wrong string',
              expected: 'ClassProperty',
              actual: 'PropertyDefinition'
            }
          },
        },
        {
          content: 'class Foo {set fooProp(value:number){}}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 36,
              actual: 22,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 36,
              actual: 22,
            },
          },
        },
        'class Foo<T> {}',
        {
          content: 'class Foo<T> { bar<U>():number { return 42; }}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 31,
              actual: 18,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 31,
              actual: 18,
            },
          },
        },
        'class Foo<T> extends Bar<T> {}',
        'class Foo<T> extends mixin(Bar) {}',
        'function foo(): {} {}',
        'function foo():(_:bool) => number{}',
        'function foo():(_?:bool) => number{}',
        'function foo(callback: (_1:bool, _2:string) => number){}',
        'function foo(callback: (_:bool) => number){}',
        'function foo(callback: () => number){}',
        'function foo(callback: () => void){}',
        'function foo(nullableNum: ?number){}',
        'function foo():number{}',
        'function foo(numVal: any){}',
        'function foo(numVal: number){}',
        'function foo(numVal: number, strVal: string){}',
        'function foo(numVal: number, untypedVal){}',
        'function foo(numVal: number, x: number){}',
        'function foo(requiredParam, optParam?) {}',
        'function foo(requiredParam, optParam?=123) {}',
        'function foo<T>() {}',
        'function foo<T,S>() {}',
        'function foo(...typedRest: Array<number>){}',
        'function foo(untypedVal, numVal: number){}',
        'function foo():() => void{}',
        'function foo([x]: Array<string>) {}',
        'var a:Array<number> = [1, 2, 3]',
        'var a:(...rest:Array<number>) => number',
        'var bar: (str:number, i:number)=> string = foo;',
        'var numVal:number;',
        'var numVal:number = otherNumVal;',
        'var [x]: Array<string> = [ "hello" ];',
        'var x : number | string = 4;',
        'var x : number & string = 4;',
        'var x : () => number | () => string = fn;',
        'var x : () => number & () => string = fn;',
        {
          content: 'var x: typeof Y | number = Y;',
          explanation: "Esprima-fb sees typeof arguments as arbitrary types",
          expected_differences: {
            'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.types.0.argument.type': {
              type: 'Wrong string',
              expected: 'GenericTypeAnnotation',
              actual: 'Identifier',
            },
            'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.types.0.argument.id': {
              type: 'Missing property'
            },
            'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.types.0.argument.typeParameters': {
              type: 'Missing property'
            },
            'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.types.0.argument.name': {
              type: 'Unexpected property'
            },
            'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.types.0.argument.typeAnnotation': {
              type: 'Unexpected property'
            },
            'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.types.0.argument.optional': {
              type: 'Unexpected property'
            },
          },
        },
        {
          content: 'var x: typeof Y & number = Y;',
          explanation: "Esprima-fb sees typeof arguments as arbitrary types",
          expected_differences: {
            'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.types.0.argument.type': {
              type: 'Wrong string',
              expected: 'GenericTypeAnnotation',
              actual: 'Identifier',
            },
            'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.types.0.argument.id': {
              type: 'Missing property'
            },
            'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.types.0.argument.typeParameters': {
              type: 'Missing property'
            },
            'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.types.0.argument.name': {
              type: 'Unexpected property'
            },
            'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.types.0.argument.typeAnnotation': {
              type: 'Unexpected property'
            },
            'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.types.0.argument.optional': {
              type: 'Unexpected property'
            },
          },
        },
        {
          content: 'var x: typeof Y = Y;',
          explanation: "Esprima-fb sees typeof arguments as arbitrary types",
          expected_differences: {
            'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.argument.type': {
              type: 'Wrong string',
              expected: 'GenericTypeAnnotation',
              actual: 'Identifier',
            },
            'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.argument.id': {
              type: 'Missing property'
            },
            'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.argument.typeParameters': {
              type: 'Missing property'
            },
            'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.argument.name': {
              type: 'Unexpected property'
            },
            'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.argument.typeAnnotation': {
              type: 'Unexpected property'
            },
            'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.argument.optional': {
              type: 'Unexpected property'
            },
          },
        },
    ],
    'Tuples': [ /* TODO */ ],
    'Type Aliases': [
      'type FBID = number;',
      'type FBID = number',
      'type Arr<T> = Array<T>;',
    ],
    'Interfaces': [
      'interface A {}',
      'interface A<T, S> {}',
      'interface A extends B {}',
      'interface A extends B, C {}',
      'interface A<T> extends B<T> {}',
      'class Foo implements Bar {}',
      'class Foo extends Bar implements Bat, Man<number> {}',
      'class Foo extends class Bar implements Bat {} {}',
      'class Foo extends class Bar implements Bat {} implements Man {}',
    ],
    'Array Types': [
      'var x: number[];',
      'var x: ?number[];',
      {
        content: 'var x: (?number)[];',
        explanation: 'Flow does not count parens in its locs',
        expected_differences: {
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.loc.start.column': {
            type: 'Wrong number',
            expected: 7,
            actual: 8,
          },
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.range.0': {
            type: 'Wrong number',
            expected: 7,
            actual: 8,
          }
        }
      },
      'var x: () => number[];',
      {
        content: 'var x: (() => number)[];',
        explanation: 'Flow does not count parens in its locs',
        expected_differences: {
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.loc.start.column': {
            type: 'Wrong number',
            expected: 7,
            actual: 8,
          },
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.range.0': {
            type: 'Wrong number',
            expected: 7,
            actual: 8,
          }
        }
      },
      {
        content: 'var x: typeof A[];',
        explanation: "Esprima-fb sees typeof arguments as arbitrary types",
        expected_differences: {
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.elementType.argument.type': {
            type: 'Wrong string',
            expected: 'GenericTypeAnnotation',
            actual: 'Identifier',
          },
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.elementType.argument.id': {
            type: 'Missing property'
          },
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.elementType.argument.typeParameters': {
            type: 'Missing property'
          },
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.elementType.argument.name': {
            type: 'Unexpected property'
          },
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.elementType.argument.typeAnnotation': {
            type: 'Unexpected property'
          },
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.elementType.argument.optional': {
            type: 'Unexpected property'
          },
        },
      },
    ],
    'Export': {
      'options': { sourceType: "module" },
      'tests': [
        {
          content: 'export var x, y',
          explanation: 'esprima-fb is outdated',
          expected_differences: {
            'root.body.0.type': {
              type: 'Wrong string',
              expected: 'ExportDeclaration',
              actual: 'ExportNamedDeclaration',
            },
            'root.body.0.default': {
              type: 'Missing property',
            },
          },
        },
        {
          content: 'export let x, y',
          explanation: 'esprima-fb is outdated',
          expected_differences: {
            'root.body.0.type': {
              type: 'Wrong string',
              expected: 'ExportDeclaration',
              actual: 'ExportNamedDeclaration',
            },
            'root.body.0.default': {
              type: 'Missing property',
            },
          },
        },
        'export const x, y',
        {
          content: 'export class A {}',
          explanation: 'esprima-fb is outdated',
          expected_differences: {
            'root.body.0.type': {
              type: 'Wrong string',
              expected: 'ExportDeclaration',
              actual: 'ExportNamedDeclaration',
            },
            'root.body.0.default': {
              type: 'Missing property',
            },
          },
        },
        {
          content: 'export default 1 + 1;',
          explanation: 'esprima-fb is outdated',
          expected_differences: {
            'root.body.0.type': {
              type: 'Wrong string',
              expected: 'ExportDeclaration',
              actual: 'ExportDefaultDeclaration',
            },
            'root.body.0.default': {
              type: 'Missing property',
            },
            'root.body.0.specifiers': {
              type: 'Missing property',
            },
            'root.body.0.source': {
              type: 'Missing property',
            },
          },
        },
        {
          content: 'export default 1 + 1',
          explanation: 'esprima-fb is outdated',
          expected_differences: {
            'root.body.0.type': {
              type: 'Wrong string',
              expected: 'ExportDeclaration',
              actual: 'ExportDefaultDeclaration',
            },
            'root.body.0.default': {
              type: 'Missing property',
            },
            'root.body.0.specifiers': {
              type: 'Missing property',
            },
            'root.body.0.source': {
              type: 'Missing property',
            },
          },
        },
        /* Esprima parses default exports wrong
        'export default function foo() {}',
        'export default function *foo() {}',
        'export default class {};',
        'export default class A {};',
        */
      ]
    },
    'Import': {
      'options': { sourceType: "module" },
      'tests': [
        'import defaultbinding, {} from "MyModule";',
        /* TODO Esprima should support these
        'import defaultbinding, {x,} from "MyModule";',
        */
        'import defaultbinding, {x,y} from "MyModule";',
        'import defaultbinding, {x as z} from "MyModule";',
        'import defaultbinding, {x, y as z} from "MyModule";',

        // These aren't import types
        'import type from "MyModule"',
        'import type, {} from "MyModule"',
        'import type, * as namespace from "MyModule"',
        'import {type} from "MyModule"',
        'import {type as type} from "MyModule"',

        // Other pseudo keywords
        'import of from "MyModule"',
        'import {of} from "MyModule"',
        'import declare from "MyModule"',
        'import {declare} from "MyModule"',
        'import async from "MyModule"',
        'import {async} from "MyModule"',

        'import {of as of} from "MyModule"',
        'import {declare as declare} from "MyModule"',
        'import {async as async} from "MyModule"',
      ],
    },
    'Import Type': {
      'options': { sourceType: "module" },
      'tests': [
        'import type defaultbinding from "MyModule";',
        /* TODO This is invalid; Esprima should also error
        'import type * as namespace from "MyModule";',
        */
        'import type {} from "MyModule";',
        'import type defaultbinding, {} from "MyModule";',
        /* TODO Esprima should support these
        'import type {x,} from "MyModule";',
        'import type defaultbinding, {x,} from "MyModule";',
        */
        'import type {x} from "MyModule";',
        'import type {x,y} from "MyModule";',
        'import type {x as z} from "MyModule";',
        'import type {x, y as z} from "MyModule";',
        'import type defaultbinding, * as namespace from "MyModule";',
        'import type defaultbinding, {x} from "MyModule";',
        'import type defaultbinding, {x,y} from "MyModule";',
        'import type defaultbinding, {x as z} from "MyModule";',
        'import type defaultbinding, {x, y as z} from "MyModule";',
      ],
    },
    'Import Typeof': {
      'options': { sourceType: "module" },
      'tests': [
        'import typeof defaultbinding from "MyModule";',
        'import typeof {} from "MyModule";',
        'import typeof defaultbinding, {} from "MyModule";',
        /* TODO Esprima should support these
        'import typeof {x,} from "MyModule";',
        'import typeof defaultbinding, {x,} from "MyModule";',
        */
        'import typeof {x} from "MyModule";',
        'import typeof {x,y} from "MyModule";',
        'import typeof {x as z} from "MyModule";',
        'import typeof {x, y as z} from "MyModule";',
        'import typeof defaultbinding, {x} from "MyModule";',
        'import typeof defaultbinding, {x,y} from "MyModule";',
        'import typeof defaultbinding, {x as z} from "MyModule";',
        'import typeof defaultbinding, {x, y as z} from "MyModule";',
      ],
    },
    'Declare Statements': [
      'declare var foo: number;',
      'declare function foo(): void',
      'declare function foo(): void;',
      'declare function foo<T>(): void;',
      'declare function foo(x: number, y: string): void;',
    ],
    'Invalid Declare Statements': [
      {
        content: 'declare function foo();',
        explanation: "Improved error message",
        expected_differences: {
          'root.errors.0.message': {
            type: 'Wrong error message',
            expected: 'Unexpected token ;',
            actual: 'Unexpected token `;`, expected the token `:`'
          }
        }
      },
    ],
    'Declare Module': [
      'declare module A {}',
      'declare module "./a/b.js" {}',
      'declare module A { declare var x: number; }',
      'declare module A { declare function foo(): number; }',
    ],
    'Invalid Declare Module': [
      {
        content: 'declare Module A {}',
        explanation: "Improved error message",
        expected_differences: {
          'root.errors.0.message': {
            type: 'Wrong error message',
            expected: 'Unexpected identifier',
            actual: 'Unexpected identifier, expected the end of an expression statement (`;`)'
          }
        }
      },
      {
        content: 'declare module {}',
        explanation: "Improved error message",
        expected_differences: {
          'root.errors.0.message': {
            type: 'Wrong error message',
            expected: 'Unexpected token {',
            actual: 'Unexpected token `{`, expected an identifier'
          }
        }
      },
      '"use strict"; declare module "\\01" {}',
      {
        content: 'declare module A { declare module B {} }',
        explanation: "We realize the error as soon as we see the B",
        expected_differences: {
          'root.errors.0.column': {
            type: 'Wrong error column',
            expected: 19,
            actual: '34-35'
          },
          'root.errors.0.message': {
            type: 'Wrong error message',
            expected: 'Unexpected identifier',
            actual: 'Unexpected identifier, expected the token `.`'
          }
        }
      },
      {
        content: 'declare module A { export default 1 +1; }',
        explanation: 'export is no longer a future reserved word',
        expected_differences: {
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Unexpected reserved word',
              actual: 'Unexpected token `export`, expected the token `declare`',
            },
        },
      },
    ],
    'Type Grouping': [
      'var a: (number)',
      {
        content: 'var a: (() => number) | () => string',
        explanation: 'Esprima counts the parens in the type locs',
        expected_differences: {
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.range.0': {
            type: 'Wrong number',
            expected: 7,
            actual: 8
          },
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.loc.start.column': {
            type: 'Wrong number',
            expected: 7,
            actual: 8
          }
        }
      },
      'var a: (A | B)',
      {
        content: 'var a: (typeof A)',
        explanation: "Esprima-fb sees typeof arguments as arbitrary types",
        expected_differences: {
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.argument.type': {
            type: 'Wrong string',
            expected: 'GenericTypeAnnotation',
            actual: 'Identifier',
          },
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.argument.id': {
            type: 'Missing property'
          },
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.argument.typeParameters': {
            type: 'Missing property'
          },
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.argument.name': {
            type: 'Unexpected property'
          },
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.argument.typeAnnotation': {
            type: 'Unexpected property'
          },
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.argument.optional': {
            type: 'Unexpected property'
          },
        },
      },
      'var a: Array<(number)>',
      {
        content: 'var a: ([]) = []',
        explanation: 'Different output for tuples - `elementTypes` is used instead of `types`',
        expected_differences: {
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.types': {
            type: 'Missing property'
          },
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.elementTypes': {
            type: 'Unexpected property'
          }
        }
      },
      'var a: (A)',
      'var a: (A.B)',
      'var a: (A<T>)',
      'var a: (A | B)',
      'var a: (A & B)',
    ],
    'Bounded Polymorphism': [
      'function foo<T: Foo>() {}',
      {
        content: 'class Foo<T: Bar>() {}',
        explanation: "Improved error message",
        expected_differences: {
          'root.errors.0.message': {
            type: 'Wrong error message',
            expected: 'Unexpected token (',
            actual: 'Unexpected token `(`, expected the token `{`'
          }
        }
      },
    ],
    'Async/Await': [
        'try { foo(); } catch (async) { bar(); }',
        'try { foo(); } catch (await) { bar(); }',
        {
          content: 'var x = { async() { bar(); }}',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.declarations.0.init.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 18,
              actual: 15,
            },
            'root.body.0.declarations.0.init.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 18,
              actual: 15,
            },
          },
        },
        {
          content: 'var x = { set async(v) { }, get async() { return "foo";}, }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.declarations.0.init.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 23,
              actual: 19,
            },
            'root.body.0.declarations.0.init.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 23,
              actual: 19,
            },
            'root.body.0.declarations.0.init.properties.1.value.range.0': {
              type: 'Wrong number',
              expected: 40,
              actual: 37,
            },
            'root.body.0.declarations.0.init.properties.1.value.loc.start.column': {
              type: 'Wrong number',
              expected: 40,
              actual: 37,
            },
          },
        },
        {
          content: 'var x = { set await(v) { }, get await() { return "foo";}, }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.declarations.0.init.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 23,
              actual: 19,
            },
            'root.body.0.declarations.0.init.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 23,
              actual: 19,
            },
            'root.body.0.declarations.0.init.properties.1.value.range.0': {
              type: 'Wrong number',
              expected: 40,
              actual: 37,
            },
            'root.body.0.declarations.0.init.properties.1.value.loc.start.column': {
              type: 'Wrong number',
              expected: 40,
              actual: 37,
            },
          },
        },
        'class async { }',
        {
          content: 'class async { async() { } }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 22,
              actual: 19,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 22,
              actual: 19,
            },
          },
        },
        {
          content: 'class async { async async() { await foo; } }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 28,
              actual: 25,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 28,
              actual: 25,
            },
          },
        },
        {
          content: 'class await { await() { } }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 22,
              actual: 19,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 22,
              actual: 19,
            },
          },
        },
        {
          content: 'class A { set async(v) { } get async() { return "foo";} }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 23,
              actual: 19,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 23,
              actual: 19,
            },
            'root.body.0.body.body.1.value.range.0': {
              type: 'Wrong number',
              expected: 39,
              actual: 36,
            },
            'root.body.0.body.body.1.value.loc.start.column': {
              type: 'Wrong number',
              expected: 39,
              actual: 36,
            },
          },
        },
        {
          content: 'class A { set await(v) { } get await() { return "foo";} }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 23,
              actual: 19,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 23,
              actual: 19,
            },
            'root.body.0.body.body.1.value.range.0': {
              type: 'Wrong number',
              expected: 39,
              actual: 36,
            },
            'root.body.0.body.body.1.value.loc.start.column': {
              type: 'Wrong number',
              expected: 39,
              actual: 36,
            },
          },
        },
        {
          content: 'y = async function() { return await bar; } ()',
          explanation: 'Babel has no problem with this, and it seems ' +
            'perfectly sensical',
          expected_differences: {
            'root.errors': {
              type: 'Flow found no error',
              expected: 'Line 1: Unexpected token (',
              actual: undefined,
            },
          },
        },
        'async function f() { return 1; }',
        'async function foo() { await 1; }',
        {
          content: 'var x = { async m() { await 1; } };',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.declarations.0.init.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 20,
              actual: 17,
            },
            'root.body.0.declarations.0.init.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 20,
              actual: 17,
            },
          },
        },
        'function async() { }',
        {
          content: 'async function foo() { return await foo + await bar + 5; }',
          explanation: 'Works in Babel and the spec appears to allow it',
          expected_differences: {
            'root.errors': {
              type: 'Flow found no error',
              expected: 'Line 1: Unexpected identifier',
              actual: undefined,
            },
          },
        },
        'var x = async function bar() { await foo; }',
        {
          content: 'async function foo() { return await; }',
          explanation: "Improved error message",
          expected_differences: {
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Unexpected token ;',
              actual: 'Unexpected token `;`'
            }
          }
        },
        'var x = async (a, b) => await a;',
        'var x = async a => await a;',
        'foo(async () => await bar);',
        'var x = async\ny => y',
        {
          content: 'class A { async bar() { await foo; } }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 22,
              actual: 19,
            },
            'root.body.0.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 22,
              actual: 19,
            },
          },
        },
        {
          content: 'var x = { async "foo"() { await y; } }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.declarations.0.init.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 24,
              actual: 21,
            },
            'root.body.0.declarations.0.init.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 24,
              actual: 21,
            },
          },
        },
        {
          content: 'var x = { async 123() { await y; } }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.declarations.0.init.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 22,
              actual: 19,
            },
            'root.body.0.declarations.0.init.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 22,
              actual: 19,
            },
          },
        },
    ],
  }
};
