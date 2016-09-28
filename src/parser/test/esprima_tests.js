module.exports = {
  todo: {
    "Whitespace": true,
    'Invalid unicode related syntax': true,
    'Invalid Type Annotations': true,
    'Array Comprehension': true,
    'Harmony: Modules': true,
    'Harmony: Invalid Class (strawman)': true,
    'ES6: Destructured Parameters': true,
    'ES7 Proposal: Rest Properties' : true,
    'Harmony Invalid syntax': true,
  },
  sections: {
    'Primary Expression': [
        'this\n',
        'null\n',
        '\n    42\n\n',
        '(1 + 2 ) * 3',
    ],

    'Grouping Operator': [
        '(1) + (2  ) + 3',
        '4 + 5 << (6)',
    ],

    'Array Initializer': [

        'x = []',
        'x = [ ]',
        'x = [ 42 ]',
        'x = [ 42, ]',
        'x = [ ,, 42 ]',
        'x = [ 1, 2, 3, ]',
        'x = [ 1, 2,, 3, ]',
        /* TODO Unicode support
        '日本語 = []',
        'T\u203F = []',
        'T\u200C = []',
        'T\u200D = []',
        '\u2163\u2161 = []',
        {
          content: '\u2163\u2161\u200A=\u2009[]',
          explanation: "TODO: support unicode character classes. Flow " +
            "doesn't realize when the LHS identifier ends due to whitespace " +
            "unicode.",
          expected_differences: {
            'root.body.0.expression.left.loc.end.column': {
              type: 'Wrong number',
              expected: 2,
              actual: 3
            },
            'root.body.0.expression.left.name': {
              type: 'Wrong string',
              expected: 'ⅣⅡ',
              actual: 'ⅣⅡ '
            }
          }
        }
        */
    ],

    'Object Initializer': [

        'x = {}',
        'x = { }',
        'x = { answer: 42 }',
        'x = { if: 42 }',
        'x = { true: 42 }',
        'x = { false: 42 }',
        'x = { null: 42 }',
        'x = { "answer": 42 }',
        'x = { x: 1, x: 2 }',
        {
          content: 'x = { get width() { return m_width } }',
          explanation: "Esprima counts the whitespace before the implicit " +
            "semicolon in its loc. Flow doesn't. Esprima-fb doesn't include " +
            "the params in the FunctionExpression.",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 35,
              actual: 34
            },
            'root.body.0.expression.right.properties.0.value.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 35,
              actual: 34
            },
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 18,
              actual: 15,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 18,
              actual: 15,
            },
          }
        },
        {
          content: 'x = { get undef() {} }',
          explanation: "Esprima-fb doesn't include the params in the FunctionExpression.",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 18,
              actual: 15,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 18,
              actual: 15,
            },
          }
        },
        {
          content: 'x = { get if() {} }',
          explanation: "Esprima-fb doesn't include the params in the FunctionExpression.",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 15,
              actual: 12,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 15,
              actual: 12,
            },
          },
        },
        {
          content: 'x = { get true() {} }',
          explanation: "Esprima-fb doesn't include the params in the FunctionExpression.",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 17,
              actual: 14,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 17,
              actual: 14,
            },
          },
        },
        {
          content: 'x = { get false() {} }',
          explanation: "Esprima-fb doesn't include the params in the FunctionExpression.",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 18,
              actual: 15,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 18,
              actual: 15,
            },
          },
        },
        {
          content: 'x = { get null() {} }',
          explanation: "Esprima-fb doesn't include the params in the FunctionExpression.",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 17,
              actual: 14,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 17,
              actual: 14,
            },
          },
        },
        {
          content: 'x = { get "undef"() {} }',
          explanation: "Esprima-fb doesn't include the params in the FunctionExpression.",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 20,
              actual: 17,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 20,
              actual: 17,
            },
          },
        },
        {
          content: 'x = { get 10() {} }',
          explanation: "Esprima-fb doesn't include the params in the FunctionExpression.",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 15,
              actual: 12,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 15,
              actual: 12,
            },
          },
        },
        {
          content: 'x = { set width(w) { m_width = w } }',
          explanation: "Esprima counts the whitespace before the implicit " +
            "semicolon in its loc. Flow doesn't. Esprima-fb doesn't include " +
            "the params in the FunctionExpression.",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 33,
              actual: 32
            },
            'root.body.0.expression.right.properties.0.value.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 33,
              actual: 32
            },
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 19,
              actual: 15,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 19,
              actual: 15,
            },
          }
        },
        {
          content: 'x = { set if(w) { m_if = w } }',
          explanation: "Esprima counts the whitespace before the implicit " +
            "semicolon in its loc. Flow doesn't. Esprima-fb doesn't include " +
            "the params in the FunctionExpression.",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 27,
              actual: 26
            },
            'root.body.0.expression.right.properties.0.value.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 27,
              actual: 26
            },
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 16,
              actual: 12,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 16,
              actual: 12,
            },
          }
        },
        {
          content: 'x = { set true(w) { m_true = w } }',
          explanation: "Esprima counts the whitespace before the implicit " +
            "semicolon in its loc. Flow doesn't. Esprima-fb doesn't include " +
            "the params in the FunctionExpression.",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 31,
              actual: 30
            },
            'root.body.0.expression.right.properties.0.value.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 31,
              actual: 30
            },
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 18,
              actual: 14,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 18,
              actual: 14,
            },
          }
        },
        {
          content: 'x = { set false(w) { m_false = w } }',
          explanation: "Esprima counts the whitespace before the implicit " +
            "semicolon in its loc. Flow doesn't. Esprima-fb doesn't include " +
            "the params in the FunctionExpression.",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 33,
              actual: 32
            },
            'root.body.0.expression.right.properties.0.value.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 33,
              actual: 32
            },
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 19,
              actual: 15,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 19,
              actual: 15,
            },
          }
        },
        {
          content: 'x = { set null(w) { m_null = w } }',
          explanation: "Esprima counts the whitespace before the implicit " +
            "semicolon in its loc. Flow doesn't. Esprima-fb doesn't include " +
            "the params in the FunctionExpression.",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 31,
              actual: 30
            },
            'root.body.0.expression.right.properties.0.value.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 31,
              actual: 30
            },
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 18,
              actual: 14,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 18,
              actual: 14,
            },
          }
        },
        {
          content: 'x = { set "null"(w) { m_null = w } }',
          explanation: "Esprima counts the whitespace before the implicit " +
            "semicolon in its loc. Flow doesn't. Esprima-fb doesn't include " +
            "the params in the FunctionExpression.",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 33,
              actual: 32
            },
            'root.body.0.expression.right.properties.0.value.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 33,
              actual: 32
            },
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 20,
              actual: 16,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 20,
              actual: 16,
            },
          }
        },
        {
          content: 'x = { set 10(w) { m_null = w } }',
          explanation: "Esprima counts the whitespace before the implicit " +
            "semicolon in its loc. Flow doesn't. Esprima-fb doesn't include " +
            "the params in the FunctionExpression.",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 29,
              actual: 28
            },
            'root.body.0.expression.right.properties.0.value.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 29,
              actual: 28
            },
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 16,
              actual: 12,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 16,
              actual: 12,
            },
          }
        },
        'x = { get: 42 }',
        'x = { set: 43 }',
        'x = { __proto__: 2 }',
        'x = {"__proto__": 2 }',
        {
          content: 'x = { get width() { return m_width }, set width(width) { m_width = width; } }',
          explanation: "Esprima counts the whitespace before the implicit " +
            "semicolon in its loc. Flow doesn't. Esprima-fb doesn't include " +
            "the params in the FunctionExpression.",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 35,
              actual: 34
            },
            'root.body.0.expression.right.properties.0.value.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 35,
              actual: 34
            },
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 18,
              actual: 15,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 18,
              actual: 15,
            },
            'root.body.0.expression.right.properties.1.value.range.0': {
              type: 'Wrong number',
              expected: 55,
              actual: 47,
            },
            'root.body.0.expression.right.properties.1.value.loc.start.column': {
              type: 'Wrong number',
              expected: 55,
              actual: 47,
            },
          }
        },

    ],

    'Comments': [

        '/* block comment */ 42',
        {
          content: '42 /* block comment 1 */ /* block comment 2 */',
          explanation: "Esprima counts comments in its loc, Flow doesn't",
          expected_differences: {
            'root.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 46,
              actual: 2
            },
            'root.loc.end.column': {
              type: 'Wrong number',
              expected: 46,
              actual: 2
            },
            'root.body.0.range.1': {
              type: 'Wrong number',
              expected: 46,
              actual: 2
            },
            'root.range.1': {
              type: 'Wrong number',
              expected: 46,
              actual: 2
            },
          }
        },
        'var p1;/* block comment 1 */ /* block comment 2 */',
        '/*42*/',
        '(a + /* assignmenr */b ) * c',
        '/* assignmenr */\n a = b',
        {
          content: '42 /*The*/ /*Answer*/',
          explanation: "Esprima counts comments in its loc, Flow doesn't",
          expected_differences: {
            'root.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 21,
              actual: 2
            },
            'root.loc.end.column': {
              type: 'Wrong number',
              expected: 21,
              actual: 2
            },
            'root.body.0.range.1': {
              type: 'Wrong number',
              expected: 21,
              actual: 2
            },
            'root.range.1': {
              type: 'Wrong number',
              expected: 21,
              actual: 2
            },
          }
        },
        {
          content: '42 /*the*/ /*answer*/',
          explanation: "Esprima counts comments in its loc, Flow doesn't",
          expected_differences: {
            'root.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 21,
              actual: 2
            },
            'root.loc.end.column': {
              type: 'Wrong number',
              expected: 21,
              actual: 2
            },
            'root.body.0.range.1': {
              type: 'Wrong number',
              expected: 21,
              actual: 2
            },
            'root.range.1': {
              type: 'Wrong number',
              expected: 21,
              actual: 2
            },
          }
        },
        {
          content: '42 /* the * answer */',
          explanation: "Esprima counts comments in its loc, Flow doesn't",
          expected_differences: {
            'root.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 21,
              actual: 2
            },
            'root.loc.end.column': {
              type: 'Wrong number',
              expected: 21,
              actual: 2
            },
            'root.body.0.range.1': {
              type: 'Wrong number',
              expected: 21,
              actual: 2
            },
            'root.range.1': {
              type: 'Wrong number',
              expected: 21,
              actual: 2
            },
          }
        },
        {
          content: '42 /* The * answer */',
          explanation: "Esprima counts comments in its loc, Flow doesn't",
          expected_differences: {
            'root.body.0.range.1': {
              type: 'Wrong number',
              expected: 21,
              actual: 2
            },
            'root.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 21,
              actual: 2
            },
            'root.range.1': {
              type: 'Wrong number',
              expected: 21,
              actual: 2
            },
            'root.loc.end.column': {
              type: 'Wrong number',
              expected: 21,
              actual: 2
            },
          }
        },
        '/* multiline\ncomment\nshould\nbe\nignored */ 42',
        '/*a\r\nb*/ 42',
        {
          content: '/*a\rb*/ 42',
          explanation: "Flow and esprima disagree whether \r is a newline",
          expected_differences: {
            'root.body.0.expression.loc.start.line': {
              type: 'Wrong number',
              expected: 2,
              actual: 1
            },
            'root.body.0.expression.loc.start.column': {
              type: 'Wrong number',
              expected: 4,
              actual: 8
            },
            'root.body.0.expression.loc.end.line': {
              type: 'Wrong number',
              expected: 2,
              actual: 1
            },
            'root.body.0.expression.loc.end.column': {
              type: 'Wrong number',
              expected: 6,
              actual: 10
            },
            'root.body.0.loc.start.line': {
              type: 'Wrong number',
              expected: 2,
              actual: 1
            },
            'root.body.0.loc.start.column': {
              type: 'Wrong number',
              expected: 4,
              actual: 8
            },
            'root.body.0.loc.end.line': {
              type: 'Wrong number',
              expected: 2,
              actual: 1
            },
            'root.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 6,
              actual: 10
            },
            'root.loc.start.line': {
              type: 'Wrong number',
              expected: 2,
              actual: 1
            },
            'root.loc.start.column': {
              type: 'Wrong number',
              expected: 4,
              actual: 8
            },
            'root.loc.end.line': {
              type: 'Wrong number',
              expected: 2,
              actual: 1
            },
            'root.loc.end.column': {
              type: 'Wrong number',
              expected: 6,
              actual: 10
            },
            'root.comments.0.loc.end.line': {
              type: 'Wrong number',
              expected: 2,
              actual: 1
            },
            'root.comments.0.loc.end.column': {
              type: 'Wrong number',
              expected: 3,
              actual: 7
            },
          }
        },
        '/*a\nb*/ 42',
        '/*a\nc*/ 42',
        '// one\\n',
        '// line comment\n42',
        {
          content: '42 // line comment',
          explanation: "Esprima counts comments in its loc, Flow doesn't",
          expected_differences: {
            'root.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 18,
              actual: 2
            },
            'root.loc.end.column': {
              type: 'Wrong number',
              expected: 18,
              actual: 2
            },
            'root.body.0.range.1': {
              type: 'Wrong number',
              expected: 18,
              actual: 2
            },
            'root.range.1': {
              type: 'Wrong number',
              expected: 18,
              actual: 2
            },
          }
        },
        '// Hello, world!\n42',
        '// Hello, world!\n',
        '// Hallo, world!\n',
        '//\n42',
        '//',
        '// ',
        '/**/42',
        {
          content: '42/**/',
          explanation: "Esprima counts comments in its loc, Flow doesn't",
          expected_differences: {
            'root.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 6,
              actual: 2
            },
            'root.loc.end.column': {
              type: 'Wrong number',
              expected: 6,
              actual: 2
            },
            'root.body.0.range.1': {
              type: 'Wrong number',
              expected: 6,
              actual: 2
            },
            'root.range.1': {
              type: 'Wrong number',
              expected: 6,
              actual: 2
            },
          }
        },
        '// Hello, world!\n\n//   Another hello\n42',
        'if (x) { doThat() // Some comment\n }',
        'if (x) { // Some comment\ndoThat(); }',
        {
          content: 'if (x) { /* Some comment */ doThat() }',
          explanation: "Esprima counts comments in its loc, Flow doesn't",
          expected_differences: {
            'root.body.0.consequent.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 37,
              actual: 36
            },
            'root.body.0.consequent.body.0.range.1': {
              type: 'Wrong number',
              expected: 37,
              actual: 36
            },
          }
        },
        {
          content: 'if (x) { doThat() /* Some comment */ }',
          explanation: "Esprima counts comments in its loc, Flow doesn't",
          expected_differences: {
            'root.body.0.consequent.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 37,
              actual: 17
            },
            'root.body.0.consequent.body.0.range.1': {
              type: 'Wrong number',
              expected: 37,
              actual: 17
            },
          }
        },
        {
          content: 'switch (answer) { case 42: /* perfect */ bingo() }',
          explanation: "Esprima counts comments in its loc, Flow doesn't",
          expected_differences: {
            'root.body.0.cases.0.consequent.0.loc.end.column': {
              type: 'Wrong number',
              expected: 49,
              actual: 48
            },
            'root.body.0.cases.0.loc.end.column': {
              type: 'Wrong number',
              expected: 49,
              actual: 48
            },
            'root.body.0.cases.0.consequent.0.range.1': {
              type: 'Wrong number',
              expected: 49,
              actual: 48
            },
            'root.body.0.cases.0.range.1': {
              type: 'Wrong number',
              expected: 49,
              actual: 48
            },
          }
        },
        {
          content: 'switch (answer) { case 42: bingo() /* perfect */ }',
          explanation: "Esprima counts comments in its loc, Flow doesn't",
          expected_differences: {
            'root.body.0.cases.0.consequent.0.loc.end.column': {
              type: 'Wrong number',
              expected: 49,
              actual: 34
            },
            'root.body.0.cases.0.loc.end.column': {
              type: 'Wrong number',
              expected: 49,
              actual: 34
            },
            'root.body.0.cases.0.consequent.0.range.1': {
              type: 'Wrong number',
              expected: 49,
              actual: 34
            },
            'root.body.0.cases.0.range.1': {
              type: 'Wrong number',
              expected: 49,
              actual: 34
            },
          }
        },
        '/* header */ (function(){ var version = 1; }).call(this)',
        '(function(){ var version = 1; /* sync */ }).call(this)',
        'function f() { /* infinite */ while (true) { } /* bar */ var each; }',
        /* Hmm, let's not support this crazy syntax for now, since it's not in
         * an ecma spec yet. Syntax comes from
         * http://javascript.spec.whatwg.org/#comment-syntax
         * and seems supported in V8 at least
        '<!-- foo',
        'var x = 1<!--foo',
        '--> comment',
        '<!-- comment',
        ' \t --> comment',
        ' \t /* block comment *REMOVE_ME/  --> comment',
        '/* block comment *REMOVE_ME/--> comment',
        '/* not comment*REMOVE_ME/; i-->0',
        'while (i-->0) {}',
        */
    ],

    'Numeric Literals': [

        '0',
        '42',
        '3',
        '5',
        '.14',
        '3.14159',
        '6.02214179e+23',
        '1.492417830e-10',
        '0x0',
        '0x0;',
        {
          content: '0e+100 ',
          explanation: "We don't want to include the extra space in the " +
            "expression statement's location",
          expected_differences: {
            'root.loc.end.column' : {
              type: 'Wrong number',
              expected: 7,
              actual: 6,
            },
            'root.body.0.loc.end.column' : {
              type: 'Wrong number',
              expected: 7,
              actual: 6,
            },
            'root.range.1': {
              type: 'Wrong number',
              expected: 7,
              actual: 6,
            },
            'root.body.0.range.1': {
              type: 'Wrong number',
              expected: 7,
              actual: 6,
            },
          }
        },
        '0e+100',
        '0xabc',
        '0xdef',
        '0X1A',
        '0x10',
        '0x100',
        '0X04',
        '02',
        '012',
        '0012',
    ],

    'String Literals': [

        '"Hello"',
        '"\\n\\r\\t\\v\\b\\f\\\\\\\'\\"\\0"',
        '"\\u0061"',
        '"\\x61"',
        '"Hello\\nworld"',
        '"Hello\\\nworld"',
        '"Hello\\02World"',
        '"Hello\\012World"',
        '"Hello\\122World"',
        '"Hello\\0122World"',
        '"Hello\\312World"',
        '"Hello\\412World"',
        '"Hello\\812World"',
        '"Hello\\712World"',
        '"Hello\\0World"',
        '"Hello\\\r\nworld"',
        '"Hello\\\rworld"',
        '"Hello\\1World"',
    ],

    'Regular Expression Literals': [

        'var x = /[\\\\]/',
        'var x = /[a-z]/i',
        'var x = /[x-z]/i',
        'var x = /[a-c]/i',
        'var x = /[P QR]/i',
        /* Esprima harmony is exploding on this
        'var x = /[\\]/]/',
        */
        'var x = /foo\\/bar/',
        'var x = /=([^=\\s])+/g',
        'var x = /42/g.test',

        /* String literal examples as regexps */
        '/Hello/',
        '/\\n\\r\\t\\v\\b\\f\\\\\\\'\\"\\0/',
        '/\\u0061/',
        '/\\x61/',
        '/\\u00/',
        '/\\xt/',
        '/Hello\\nworld/',
        '/Hello\\02World/',
        '/Hello\\012World/',
        '/Hello\\122World/',
        '/Hello\\0122World/',
        '/Hello\\312World/',
        '/Hello\\412World/',
        '/Hello\\812World/',
        '/Hello\\712World/',
        '/Hello\\0World/',
        '/Hello\\1World/',
        '/Hello (World)/',
    ],

    'Left-Hand-Side Expression': [

        'new Button',
        'new Button()',
        'new new foo',
        'new new foo()',
        'new foo().bar()',
        'new foo[bar]',
        'new foo.bar()',
        'new function() {}',
        "new window[(['Active'].concat('Object').join('X'))]('Microsoft.XMLHTTP');",
        'this.foo.export()',
        '( new foo).bar()',
        'foo(bar, baz)',
        '(    foo  )()',
        '(    foo  )[0]',
        'universe.milkyway',
        'universe.milkyway.solarsystem',
        'universe.milkyway.solarsystem.Earth',
        'universe[galaxyName, otherUselessName]',
        'universe[galaxyName]',
        'universe[42].galaxies',
        'universe(42).galaxies',
        'universe(42).galaxies(14, 3, 77).milkyway',
        'earth.asia.Indonesia.prepareForElection(2014)',
        'universe.if',
        'universe.true',
        'universe.false',
        'universe.null',
        'universe.type',
    ],

    'Postfix Expressions': [

        'x++',
        'x--',
        'eval++',
        'eval--',
        'arguments++',
        'arguments--',
    ],

    'Unary Operators': [

        '++x',
        '--x',
        '++eval',
        '--eval',
        '++arguments',
        '--arguments',
        '+x',
        '-x',
        '~x',
        '!x',
        'void x',
        'delete x',
        'typeof x',
    ],

    'Multiplicative Operators': [

        'x * y',
        'x / y',
        'x % y',
    ],

    'Additive Operators': [

        'x + y',
        'x - y',
        '"use strict" + 42',
    ],

    'Bitwise Shift Operator': [

        'x << y',
        'x >> y',
        'x >>> y',
    ],

    'Relational Operators': [

        'x < y',
        'x > y',
        'x <= y',
        'x >= y',
        'x in y',
        'x instanceof y',
        'x < y < z',
    ],

    'Equality Operators': [

        'x == y',
        'x != y',
        'x === y',
        'x !== y',
    ],

    'Binary Bitwise Operators': [

        'x & y',
        'x ^ y',
        'x | y',
    ],

    'Binary Expressions': [

        'x + y + z',
        'x - y + z',
        'x + y - z',
        'x - y - z',
        'x + y * z',
        'x + y / z',
        'x - y % z',
        'x * y * z',
        'x * y / z',
        'x * y % z',
        'x % y * z',
        'x << y << z',
        'x | y | z',
        'x & y & z',
        'x ^ y ^ z',
        'x & y | z',
        'x | y ^ z',
        'x | y & z',
        {
          content: '1 + type + interface + declare + let + eval + super + ' +
            'async + await',
          explanation: 'Let is fine as an identifier',
          expected_differences: {
            'root.errors': {
              type: 'Flow found no error',
              expected: 'Line 1: Unexpected token let',
              actual: undefined
            },
          }
        },
    ],

    'Binary Logical Operators': [

        'x || y',
        'x && y',
        'x || y || z',
        'x && y && z',
        'x || y && z',
        'x || y ^ z',
        '(x || y) || z',
        'x || (y || z)',
        '(x && y) || z',
        'x || (y && z)',
        '(x || y) && z',
        'x && (y || z)',
    ],

    'Conditional Operator': [

        'y ? 1 : 2',
        'x && y ? 1 : 2',
        'x = (0) ? 1 : 2',
    ],

    'Assignment Operators': [

        'x = 42',
        'eval = 42',
        'arguments = 42',
        'type = 42',
        'of = 42',
        'interface = 42',
        'declare = 42',
        'x *= 42',
        'x /= 42',
        'x %= 42',
        'x += 42',
        'x -= 42',
        'x <<= 42',
        'x >>= 42',
        'x >>>= 42',
        'x &= 42',
        'x ^= 42',
        'x |= 42',
    ],

    'Complex Expression': [

        'a || b && c | d ^ e & f == g < h >>> i + j * k',
    ],

    'Block': [

        {
          content: '{ foo }',
          explanation: "Esprima counts the whitespace before the curly brace " +
            "that implies the semicolon. Flow doesn't",
          expected_differences: {
            'root.body.0.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 6,
              actual: 5
            },
            'root.body.0.body.0.range.1': {
              type: 'Wrong number',
              expected: 6,
              actual: 5
            }
          }
        },
        '{ doThis(); doThat(); }',
        '{}',
    ],

    'Variable Statement': [

        'var x',
        'var x, y;',
        'var x = 42',
        'var eval = 42, arguments = 42, type = 12',
        'var x = 14, y = 3, z = 1977',
        'var implements, interface, declare, package',
        'var private, protected, public, static',
    ],

    'Let Statement': [

        'let x',
        {
          content: '{ let x }',
          explanation: "Esprima counts the space before the implicit " +
            "semicolon. Flow doesn't",
          expected_differences: {
            'root.body.0.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 8,
              actual: 7
            },
            'root.body.0.body.0.range.1': {
              type: 'Wrong number',
              expected: 8,
              actual: 7
            },
          }
        },
        {
          content: '{ let x = 42 }',
          explanation: "Esprima counts the space before the implicit " +
            "semicolon. Flow doesn't",
          expected_differences: {
            'root.body.0.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 13,
              actual: 12
            },
            'root.body.0.body.0.range.1': {
              type: 'Wrong number',
              expected: 13,
              actual: 12
            },
          }
        },
        {
          content: '{ let x = 14, y = 3, z = 1977 }',
          explanation: "Esprima counts the space before the implicit " +
            "semicolon. Flow doesn't",
          expected_differences: {
            'root.body.0.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 30,
              actual: 29
            },
            'root.body.0.body.0.range.1': {
              type: 'Wrong number',
              expected: 30,
              actual: 29
            },
          }
        },
        {
          content: 'if (true) let += 1',
          explanation: 'Let is fine as an identifier',
          expected_differences: {
            'root.errors': {
              type: 'Flow found no error',
              expected: 'Line 1: Unexpected token let',
              actual: undefined
            },
          }
        }
    ],

    'Const Statement': [

        'const x = 42',
        {
          content: '{ const x = 42 }',
          explanation: "Esprima counts the space before the implicit " +
            "semicolon. Flow doesn't",
          expected_differences: {
            'root.body.0.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 15,
              actual: 14
            },
            'root.body.0.body.0.range.1': {
              type: 'Wrong number',
              expected: 15,
              actual: 14
            },
          }
        },
        {
          content: '{ const x = 14, y = 3, z = 1977 }',
          explanation: "Esprima counts the space before the implicit " +
            "semicolon. Flow doesn't",
          expected_differences: {
            'root.body.0.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 32,
              actual: 31
            },
            'root.body.0.body.0.range.1': {
              type: 'Wrong number',
              expected: 32,
              actual: 31
            },
          }
        },
    ],

    'Empty Statement': [

        ';',
    ],

    'Expression Statement': [

        'x',
        'x, y',
        /* TODO Unicode :(
        '\\u0061',
        'a\\u0061',
        '\\u0061a',
        '\\u0061a ',
        */
    ],

    'If Statement': [

        'if (morning) goodMorning()',
        'if (morning) (function(){})',
        'if (morning) var x = 0;',
        'if (morning) function a(){}',
        'if (morning) goodMorning(); else goodDay()',
        /* Esprima harmony is exploding on this
        'if (true) that()\n; else;',
        */
        'if (true) that(); else;',
        'if (true) declare += 4;',
    ],

    'Iteration Statements': [

        'do keep(); while (true)',
        'do keep(); while (true);',
        'do { x++; y--; } while (x < 10)',
        {
          content: '{ do { } while (false) false }',
          explanation: "Esprima counts whitespace before implicit semicolon. " +
            "Flow does not.",
          expected_differences: {
            'root.body.0.body.1.loc.end.column': {
              type: 'Wrong number',
              expected: 29,
              actual: 28
            },
            'root.body.0.body.1.range.1': {
              type: 'Wrong number',
              expected: 29,
              actual: 28
            },
          }
        },
        'do that();while (true)',
        /* Esprima harmony is exploding on this
        'do that()\n;while (true)',
        */
        'while (true) doSomething()',
        'while (x < 10) { x++; y--; }',
        'for(;;);',
        'for(;;){}',
        'for(x = 0;;);',
        'for(var x = 0;;);',
        'for(let x = 0;;);',
        'for(var x = 0, y = 1;;);',
        'for(x = 0; x < 42;);',
        'for(x = 0; x < 42; x++);',
        'for(x = 0; x < 42; x++) process(x);',
        'for(x in list) process(x);',
        'for (var x in list) process(x);',
        /* Esprima harmony is exploding due to unexpect in
        'for (var x = 42 in list) process(x);',
        */
        'for (let x in list) process(x);',
        /* Esprima harmony is exploding due to unexpect in
        'for (var x = y = z in q);',
        */
        /* Esprima harmony is exploding due to unexpect in
        {
          content: 'for (var a = b = c = (d in e) in z);',
          explanation: "Esprima counts the parens for (d in e) in the loc. " +
            "Flow does not.",
          expected_differences: {
            'root.body.0.left.loc.end.column': {
              type: 'Wrong number',
              expected: 29,
              actual: 28
            },
            'root.body.0.left.declarations.0.loc.end.column': {
              type: 'Wrong number',
              expected: 29,
              actual: 28
            },
            'root.body.0.left.declarations.0.init.loc.end.column': {
              type: 'Wrong number',
              expected: 29,
              actual: 28
            },
            'root.body.0.left.declarations.0.init.right.loc.end.column': {
              type: 'Wrong number',
              expected: 29,
              actual: 28
            },
          }
        },
        */
        /* Esprima harmony is exploding due to unexpect in
        {
          content: 'for (var i = function() { return 10 in [] } in list) process(x);',
          explanation: "The return statement has an implicit semicolon. "+
            "Esprima counts the whitespace before the implicit semicolon in " +
            "the loc. Flow doesn't",
          expected_differences: {
            'root.body.0.left.declarations.0.init.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 42,
              actual: 41
            }
          }
        }
        */
    ],

    'continue statement': [

        'while (true) { continue; }',
        {
          content: 'while (true) { continue }',
          explanation: " Esprima counts the whitespace before the curly " +
          "brace that implies the semicolon. Flow doesn't",
          expected_differences: {
            'root.body.0.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 24,
              actual: 23
            },
            'root.body.0.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 24,
              actual: 23
            },
          }
        },
        {
          content: 'done: while (true) { continue done }',
          explanation: " Esprima counts the whitespace before the curly " +
          "brace that implies the semicolon. Flow doesn't",
          expected_differences: {
            'root.body.0.body.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 35,
              actual: 34
            },
            'root.body.0.body.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 35,
              actual: 34
            },
          }
        },
        'done: while (true) { continue done; }',
        '__proto__: while (true) { continue __proto__; }',
    ],

    'break statement': [

        {
          content: 'while (true) { break }',
          explanation: " Esprima counts the whitespace before the curly " +
          "brace that implies the semicolon. Flow doesn't",
          expected_differences: {
            'root.body.0.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 21,
              actual: 20
            },
            'root.body.0.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 21,
              actual: 20
            },
          }
        },
        {
          content: 'done: while (true) { break done }',
          explanation: " Esprima counts the whitespace before the curly " +
          "brace that implies the semicolon. Flow doesn't",
          expected_differences: {
            'root.body.0.body.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 32,
              actual: 31
            },
            'root.body.0.body.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 32,
              actual: 31
            },
          }
        },
        'done: while (true) { break done; }',
        '__proto__: while (true) { break __proto__; }',
    ],

    'return statement': [

        {
          content: '(function(){ return })',
          explanation: "Esprima counts the parens and the whitespace before " +
            "an implicit semicolon in its loc. Flow doesn't",
          expected_differences: {
            'root.body.0.expression.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 20,
              actual: 19
            },
            'root.body.0.expression.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 20,
              actual: 19
            },
          }
        },
        '(function(){ return; })',
        '(function(){ return x; })',
        {
          content: '(function(){ return x * y })',
          explanation: "Esprima counts the whitespace before " +
            "an implicit semicolon in its loc. Flow doesn't",
          expected_differences: {
            'root.body.0.expression.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 26,
              actual: 25
            },
            'root.body.0.expression.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 26,
              actual: 25
            },
          }
        },
    ],

    'with statement': [

        'with (x) foo = bar',
        'with (x) foo = bar;',
        {
          content: 'with (x) { foo = bar }',
          explanation: "Esprima counts the whitespace before the implicit "+
            "semicolon in its loc. Flow doesn't",
          expected_differences: {
            'root.body.0.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 21,
              actual: 20
            },
            'root.body.0.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 21,
              actual: 20
            },
          }
        }
    ],

    'switch statement': [

        'switch (x) {}',
        'switch (answer) { case 42: hi(); break; }',
        {
          content: 'switch (answer) { case 42: hi(); break; default: break }',
          explanation: "Esprima counts the whitespace before the implicit " +
            "semicolon in its loc. Flow doesn't",
          expected_differences: {
            'root.body.0.cases.1.loc.end.column': {
              type: 'Wrong number',
              expected: 55,
              actual: 54
            },
            'root.body.0.cases.1.consequent.0.loc.end.column': {
              type: 'Wrong number',
              expected: 55,
              actual: 54
            },
            'root.body.0.cases.1.range.1': {
              type: 'Wrong number',
              expected: 55,
              actual: 54
            },
            'root.body.0.cases.1.consequent.0.range.1': {
              type: 'Wrong number',
              expected: 55,
              actual: 54
            },
          }
        }
    ],

    'Labelled Statements': [

        'start: for (;;) break start',
        'start: while (true) break start',
        '__proto__: test',
        'type: 42',
        'of: 52',
        'declare: 62',
    ],

    'throw statement': [

        'throw x;',
        'throw x * y',
        'throw { message: "Error" }',
    ],

    'try statement': [

        'try { } catch (e) { }',
        'try { } catch (eval) { }',
        'try { } catch (arguments) { }',
        {
          content: 'try { } catch (e) { say(e) }',
          explanation: "Esprima counts the whitespace before the implicit " +
            "semicolon in its loc. Flow doesn't",
          expected_differences: {
            'root.body.0.handlers.0.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 27,
              actual: 26
            },
            'root.body.0.handlers.0.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 27,
              actual: 26
            },
          }
        },
        {
          content: 'try { } finally { cleanup(stuff) }',
          explanation: "Esprima counts the whitespace before the implicit " +
            "semicolon in its loc. Flow doesn't",
          expected_differences: {
            'root.body.0.finalizer.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 33,
              actual: 32
            },
            'root.body.0.finalizer.body.0.range.1': {
              type: 'Wrong number',
              expected: 33,
              actual: 32
            },
          }
        },
        {
          content: 'try { doThat(); } catch (e) { say(e) }',
          explanation: "Esprima counts the whitespace before the implicit " +
            "semicolon in its loc. Flow doesn't",
          expected_differences: {
            'root.body.0.handlers.0.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 37,
              actual: 36
            },
            'root.body.0.handlers.0.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 37,
              actual: 36
            },
          }
        },
        {
          content: 'try { doThat(); } catch (e) { say(e) } finally { cleanup(stuff) }',
          explanation: "Esprima counts the whitespace before the two implicit " +
            "semicolons in its loc. Flow doesn't",
          expected_differences: {
            'root.body.0.handlers.0.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 37,
              actual: 36
            },
            'root.body.0.finalizer.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 64,
              actual: 63
            },
            'root.body.0.handlers.0.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 37,
              actual: 36
            },
            'root.body.0.finalizer.body.0.range.1': {
              type: 'Wrong number',
              expected: 64,
              actual: 63
            },
          }
        },
    ],

    'debugger statement': [

        'debugger;',
        'debugger',
    ],

    'Function Expression': [
      '(function(){}())',
      'var x = function(){}.bind(this)',
    ],

    'Function Definition': [

        'function hello() { sayHi(); }',
        'function eval() { }',
        'function arguments() { }',
        'function test(t, t) { }',
        '(function test(t, t) { })',
        {
          content: 'function eval() { function inner() { "use strict" } }',
          explanation: "Esprima counts the implict semicolon in the loc. " +
            "Flow doesn't",
          expected_differences: {
            'root.body.0.body.body.0.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 50,
              actual: 49
            },
            'root.body.0.body.body.0.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 50,
              actual: 49
            },
          }
        },
        'function hello(a) { sayHi(); }',
        'function hello(a, b) { sayHi(); }',
        {
          content: 'var hi = function() { sayHi() };',
          explanation: "Esprima counts the implict semicolon in the loc. " +
            "Flow doesn't",
          expected_differences: {
            'root.body.0.declarations.0.init.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 30,
              actual: 29
            },
            'root.body.0.declarations.0.init.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 30,
              actual: 29
            },
          }
        },
        'var hi = function eval() { };',
        'var hi = function arguments() { };',
        {
          content: 'var hello = function hi() { sayHi() };',
          explanation: "Esprima counts the implict semicolon in the loc. " +
            "Flow doesn't",
          expected_differences: {
            'root.body.0.declarations.0.init.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 36,
              actual: 35
            },
            'root.body.0.declarations.0.init.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 36,
              actual: 35
            },
          }
        },
        '(function(){})',
        'function universe(__proto__) { }',
        'function test() { "use strict" + 42; }',
    ],

    '[ES6] Default Parameter Value': [

        'x = function(y = 1) {}',
        'function f(a = 1) {}',
        'x = { f: function(a=1) {} }',
    ],

    'Automatic semicolon insertion': [

        {
          content: '{ x\n++y }',
          explanation: "Esprima counts the implicit semicolon on the second "+
            "line in its loc. Flow doesn't",
          expected_differences: {
            'root.body.0.body.1.loc.end.column': {
              type: 'Wrong number',
              expected: 4,
              actual: 3
            },
            'root.body.0.body.1.range.1': {
              type: 'Wrong number',
              expected: 8,
              actual: 7
            },
          }
        },
        {
          content: '{ x\n--y }',
          explanation: "Esprima counts the implicit semicolon on the second "+
            "line in its loc. Flow doesn't",
          expected_differences: {
            'root.body.0.body.1.loc.end.column': {
              type: 'Wrong number',
              expected: 4,
              actual: 3
            },
            'root.body.0.body.1.range.1': {
              type: 'Wrong number',
              expected: 8,
              actual: 7
            },
          }
        },
        'var x /* comment */;',
        '{ var x = 14, y = 3\nz; }',
        'while (true) { continue\nthere; }',
        'while (true) { continue // Comment\nthere; }',
        'while (true) { continue /* Multiline\nComment */there; }',
        'while (true) { break\nthere; }',
        'while (true) { break // Comment\nthere; }',
        'while (true) { break /* Multiline\nComment */there; }',
        '(function(){ return\nx; })',
        '(function(){ return // Comment\nx; })',
        '(function(){ return/* Multiline\nComment */x; })',
        '{ throw error\nerror; }',
        '{ throw error// Comment\nerror; }',
        '{ throw error/* Multiline\nComment */error; }',
    ],

    'Directive Prolog': [

        '(function () { \'use\\x20strict\'; with (i); }())',
        '(function () { \'use\\nstrict\'; with (i); }())',
    ],

    'Whitespace': [

        'new\x20\x09\x0B\x0C\xA0\u1680\u180E\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u202F\u205F\u3000\uFEFFa',
        '{0\x0A1\x0D2\u20283\u20294}',
    ],

    'Source elements': [

        {
          content: '',
          explanation: "An empty program should start and end on line 1. "+
            "Ain't no such thing as line 0",
          expected_differences: {
            'root.loc.start.line': {
              type: 'Wrong number',
              expected: 0,
              actual: 1
            },
            'root.loc.end.line': {
              type: 'Wrong number',
              expected: 0,
              actual: 1
            }
          }
        },
    ],

    'Source option': [
        'x + y - z',
        'a + (b < (c * d)) + e',
    ],


    'Invalid syntax': [

        '{',
        '}',
        '3ea',
        '3in []',
        '3e',
        {
          content: '3e+',
          explanation: "Esprima counts comments in its loc, Flow doesn't",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 3,
              actual: '0-2'
            },
          }
        },
        {
          content: '3e-',
          explanation: "Esprima counts comments in its loc, Flow doesn't",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 3,
              actual: '0-2'
            },
          }
        },
        '3x',
        '3x0',
        '0x',
        {
          content: '09',
          explanation: "Spec doesn't support this but every implementation does",
          expected_differences: {
            'root.errors': {
              type: 'Flow found no error',
              expected: 'Line 1: Unexpected token ILLEGAL',
              actual: undefined
            },
          }
        },
        '018',
        '01a',
        '3in[]',
        '0x3in[]',
        '"Hello\nWorld"',
        'x\\',
        'var x = /(s/g',
        '/',
        '/test',
        '/test\n/',
        /* TODO uncomment when esprima is fixed
         * https://code.google.com/p/esprima/issues/detail?id=604
        {
          content: 'var x = /[a-z]/\\ux',
          explanation: "10.8.5.1 disallows unicode escape flags",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 18,
              actual: '15-16'
            },
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Invalid regular expression',
              actual: 'Unexpected token ILLEGAL'
            },
          }
        },
        {
          content: 'var x = /[a-z]/\\\\ux',
          explanation: "10.8.5.1 disallows unicode escape flags",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 19,
              actual: '15-16'
            },
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Invalid regular expression',
              actual: 'Unexpected token ILLEGAL'
            },
          }
        },
        */
        '3 = 4',
        'func() = 4',
        {
          content: '(1 + 1) = 10',
          explanation: "Esprima counts parens in its loc, Flow doesn't",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 7,
              actual: '1-6'
            },
          }
        },
        '"\\u{110000}"',
        {
          content: '"\\u{}"',
          explanation: "Flow's lexer complains at the beginning of a bad escape",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 4,
              actual: '2-3'
            },
          }
        },
        {
          content: '"\\u{FFFF"',
          explanation: "Flow's lexer complains at the beginning of a bad escape",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 9,
              actual: '2-3'
            },
          }
        },
        {
          content: '"\\u{FFZ}"',
          explanation: "Flow's lexer complains at the beginning of a bad escape",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 7,
              actual: '2-3'
            },
          }
        },
        'x\\',
        '1++',
        '1--',
        '++1',
        '--1',
        {
          content: 'for((1 + 1) in list) process(x);',
          explanation: "Esprima counts comments in its loc, Flow doesn't",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 11,
              actual: '5-10'
            },
          }
        },
        '[',
        '[,',
        '1 + {',
        '1 + { t:t ',
        '1 + { t:t,',
        'var x = /\n/',
        'var x = "\n',
        'var if = 42',
        'i #= 42',
        'i + 2 = 42',
        '+i = 42',
        '1 + (',
        '\n\n\n{',
        '\n/* Some multiline\ncomment */\n)',
        '{ set 1 }',
        '{ get 2 }',
        '({ set: s(if) { } })',
        '({ set s(.) { } })',
        {
          content: '({ set s() { } })',
          explanation: "Esprima error isn't great",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 9,
              actual: '7-8'
            },
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Unexpected token )',
              actual: 'Setter should have exactly one parameter'
            }
          }
        },
        '({ set: s() { } })',
        '({ set: s(a, b) { } })',
        '({ get: g(d) { } })',
        '({ get i() { }, i: 42 })',
        '({ i: 42, get i() { } })',
        '({ set i(x) { }, i: 42 })',
        '({ i: 42, set i(x) { } })',
        '({ get i() { }, get i() { } })',
        '({ set i(x) { }, set i(x) { } })',
        '((a)) => 42',
        '(a, (b)) => 42',
        {
          content: '"use strict"; (eval = 10) => 42',
          explanation: "This is an arrow function error, not an assignment "+
            "error",
          expected_differences: {
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Assignment to eval or arguments is not allowed in strict mode',
              actual: 'Parameter name eval or arguments is not allowed in strict mode',
            },
          },
        },
        // strict mode, using eval when IsSimpleParameterList is true
        {
          content: '"use strict"; eval => 42',
          explanation: "Esprima error loc is crazy here",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 24,
              actual: '14-18'
            },
          }
        },
        // strict mode, using arguments when IsSimpleParameterList is true
        {
          content: '"use strict"; arguments => 42',
          explanation: "Esprima doesn't point to the bad argument",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 29,
              actual: '14-23'
            },
          }
        },
        // strict mode, using eval when IsSimpleParameterList is true
        {
          content: '"use strict"; (eval, a) => 42',
          explanation: "Esprima error loc is crazy here",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 29,
              actual: '15-19'
            },
          }
        },
        // strict mode, using arguments when IsSimpleParameterList is true
        {
          content: '"use strict"; (arguments, a) => 42',
          explanation: "Esprima doesn't point to the bad argument",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 34,
              actual: '15-24'
            },
          }
        },
        {
          content: '"use strict"; (a, a) => 42',
          explanation: "Esprima counts parens in its loc, Flow doesn't",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 20,
              actual: '18-19'
            },
          }
        },
        '"use strict"; (a) => 00',
        '() <= 42',
        '() ? 42',
        '() + 42',
        '(...x) + 42',
        '()++',
        '()()',
        '(10) => 00',
        '(10, 20) => 00',
        {
          content: '"use strict"; (eval) => 42',
          explanation: "Esprima error loc is crazy here",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 26,
              actual: '15-19'
            },
          }
        },
        {
          content: '(eval) => { "use strict"; 42 }',
          explanation: "Esprima error loc is crazy here",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 30,
              actual: '1-5'
            },
          }
        },
        'function t(if) { }',
        'function t(true) { }',
        'function t(false) { }',
        'function t(null) { }',
        'function null() { }',
        'function true() { }',
        'function false() { }',
        'function if() { }',
        'a b;',
        'if.a;',
        'a if;',
        {
          content: 'a class;',
          explanation: 'class is no longer a future reserved word',
          expected_differences: {
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Unexpected reserved word',
              actual: 'Unexpected token class',
            },
          },
        },
        'break\n',
        'break 1;',
        'continue\n',
        'continue 2;',
        'throw',
        'throw;',
        'throw\n',
        {
          content: 'for (var i, i2 in {});',
          explanation: "Esprima counts comments in its loc, Flow doesn't",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 15,
              actual: '5-14'
            },
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Unexpected token in',
              actual: 'Invalid left-hand side in for-in'
            },
          }
        },
        'for ((i in {}));',
        'for (i + 1 in {});',
        'for (+i in {});',
        'if(false)',
        'if(false) doThis(); else',
        'do',
        'while(false)',
        'for(;;)',
        'with(x)',
        'try { }',

        'const x = 12, y;',
        'const x, y = 12;',
        'const x;',
        {
          content: 'if(true) let a = 1;',
          explanation: "The let keyword is parsed as an identifer. " +
            "It's the a that causes the error",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 9,
              actual: '13-14'
            },
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Unexpected token let',
              actual: 'Unexpected identifier',
            },
          }
        },
        'if(true) const a = 1;',
        {
          content: 'switch (c) { default: default: }',
          explanation: "Esprima points after the duplicate default",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 30,
              actual: '22-29'
            },
          }
        },
        'new X()."s"',
        '/*',
        '/*\n\n\n',
        '/**',
        '/*\n\n*',
        '/*hello',
        '/*hello  *',
        '\n]',
        {
          content: '\r]',
          explanation: "Flow and esprima disagree whether \r is a newline",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 0,
              actual: '1-2'
            },
            'root.errors.0.line': {
              type: 'Wrong error line',
              expected: 2,
              actual: '1-1'
            },
          }
        },
        '\r\n]',
        {
          content: '\n\r]',
          explanation: "Flow and esprima disagree whether \r is a newline",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 0,
              actual: '1-2'
            },
            'root.errors.0.line': {
              type: 'Wrong error line',
              expected: 3,
              actual: '2-2'
            },
          }
        },
        '//\r\n]',
        {
          content: '//\n\r]',
          explanation: "Flow and esprima disagree whether \r is a newline",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 0,
              actual: '1-2'
            },
            'root.errors.0.line': {
              type: 'Wrong error line',
              expected: 3,
              actual: '2-2'
            },
          }
        },
        '/a\\\n/',
        {
          content: '//\r \n]',
          explanation: "Flow and esprima disagree whether \r is a newline",
          expected_differences: {
            'root.errors.0.line': {
              type: 'Wrong error line',
              expected: 3,
              actual: '2-2'
            },
          }
        },
        '/*\r\n*/]',
        {
          content: '/*\n\r*/]',
          explanation: "Flow and esprima disagree whether \r is a newline",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 2,
              actual: '3-4'
            },
            'root.errors.0.line': {
              type: 'Wrong error line',
              expected: 3,
              actual: '2-2'
            },
          }
        },
        {
          content: '/*\r \n*/]',
          explanation: "Flow and esprima disagree whether \r is a newline",
          expected_differences: {
            'root.errors.0.line': {
              type: 'Wrong error line',
              expected: 3,
              actual: '2-2'
            },
          }
        },
        '\\\\',
        '\\x',
        {
          content: '"\\',
          explanation: "Esprima has a non-existant location for the eof",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 3,
              actual: '2-2'
            },
          }
        },
        '"\\u',
        'try { } catch() {}',
        'return',
        'break',
        'continue',
        'switch (x) { default: continue; }',
        'do { x } *',
        'while (true) { break x; }',
        'while (true) { continue x; }',
        'x: while (true) { (function () { break x; }); }',
        'x: while (true) { (function () { continue x; }); }',
        'x: while (true) { (function () { break; }); }',
        'x: while (true) { (function () { continue; }); }',
        {
          content: 'x: while (true) { x: while (true) { } }',
          explanation: "Esprima points to the location after the second colon",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 20,
              actual: '18-19'
            },
          }
        },
        '(function () { \'use strict\'; delete i; }())',
        '(function () { \'use strict\'; with (i); }())',
        'function hello() {\'use strict\'; ({ i: 42, i: 42 }) }',
        'function hello() {\'use strict\'; ({ hasOwnProperty: 42, hasOwnProperty: 42 }) }',
        'function hello() {\'use strict\'; var eval = 10; }',
        'function hello() {\'use strict\'; var arguments = 10; }',
        'function hello() {\'use strict\'; try { } catch (eval) { } }',
        'function hello() {\'use strict\'; try { } catch (arguments) { } }',
        'function hello() {\'use strict\'; eval = 10; }',
        'function hello() {\'use strict\'; arguments = 10; }',
        'function hello() {\'use strict\'; ++eval; }',
        'function hello() {\'use strict\'; --eval; }',
        'function hello() {\'use strict\'; ++arguments; }',
        'function hello() {\'use strict\'; --arguments; }',
        'function hello() {\'use strict\'; eval++; }',
        'function hello() {\'use strict\'; eval--; }',
        'function hello() {\'use strict\'; arguments++; }',
        'function hello() {\'use strict\'; arguments--; }',
        'function hello() {\'use strict\'; function eval() { } }',
        'function hello() {\'use strict\'; function arguments() { } }',
        'function eval() {\'use strict\'; }',
        'function arguments() {\'use strict\'; }',
        'function hello() {\'use strict\'; (function eval() { }()) }',
        'function hello() {\'use strict\'; (function arguments() { }()) }',
        '(function eval() {\'use strict\'; })()',
        '(function arguments() {\'use strict\'; })()',
        'function hello() {\'use strict\'; ({ s: function eval() { } }); }',
        '(function package() {\'use strict\'; })()',
        'function hello() {\'use strict\'; ({ i: 10, set s(eval) { } }); }',
        'function hello() {\'use strict\'; ({ set s(eval) { } }); }',
        'function hello() {\'use strict\'; ({ s: function s(eval) { } }); }',
        'function hello(eval) {\'use strict\';}',
        'function hello(arguments) {\'use strict\';}',
        'function hello() { \'use strict\'; function inner(eval) {} }',
        'function hello() { \'use strict\'; function inner(arguments) {} }',
        ' "\\1"; \'use strict\';',
        'function hello() { \'use strict\'; "\\1"; }',
        'function hello() { \'use strict\'; 021; }',
        'function hello() { \'use strict\'; ({ "\\1": 42 }); }',
        'function hello() { \'use strict\'; ({ 021: 42 }); }',
        'function hello() { "octal directive\\1"; "use strict"; }',
        'function hello() { "octal directive\\1"; "octal directive\\2"; "use strict"; }',
        'function hello() { "use strict"; function inner() { "octal directive\\1"; } }',
        'function hello() { "use strict"; var implements; }',
        'function hello() { "use strict"; var interface; }',
        'function hello() { "use strict"; var package; }',
        'function hello() { "use strict"; var private; }',
        'function hello() { "use strict"; var protected; }',
        'function hello() { "use strict"; var public; }',
        'function hello() { "use strict"; var static; }',
        'function hello() { "use strict"; var yield; }',
        'function hello() { "use strict"; var let; }',
        'function hello(static) { "use strict"; }',
        'function static() { "use strict"; }',
        'function eval(a) { "use strict"; }',
        'function arguments(a) { "use strict"; }',
        'var yield',
        {
          content: 'var let',
          explanation: "Bug in esprima. This should be allowed",
          expected_differences: {
            'root.errors': {
              type: 'Flow found no error',
              expected: 'Line 1: Unexpected token let',
              actual: undefined
            },
          }
        },
        '"use strict"; function static() { }',
        'function a(t, t) { "use strict"; }',
        'function a(eval) { "use strict"; }',
        'function a(package) { "use strict"; }',
        'function a() { "use strict"; function b(t, t) { }; }',
        '(function a(t, t) { "use strict"; })',
        'function a() { "use strict"; (function b(t, t) { }); }',
        // Duplicates are forbidden if IsSimpleParameterList is false, and rest
        // params, patterns, and defaults all make the params non-simple
        {
          content: '(t, t, ...rest) => 42',
          explanation: "Esprima points to the end of the params not to the "+
            "bad param itself",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 15,
              actual: '4-5'
            },
          }
        },
        {
          content: '(t, t, [b]) => 42',
          explanation: "Esprima points to the end of the params not to the "+
            "bad param itself",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 11,
              actual: '4-5'
            },
          }
        },
        {
          content: '(t, t, {b}) => 42',
          explanation: "Esprima points to the end of the params not to the "+
            "bad param itself",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 11,
              actual: '4-5'
            },
          }
        },
        {
          content: '(t, t, b=1) => 42',
          explanation: "Esprima points to the end of the params not to the "+
            "bad param itself",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 11,
              actual: '4-5'
            },
          }
        },
        '(function a(eval) { "use strict"; })',
        '(function a(package) { "use strict"; })',
        {
          content: '__proto__: __proto__: 42;',
          explanation: "Esprima points to the location after the second colon",
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 21,
              actual: '11-20'
            },
          }
        },
        '"use strict"; function t(__proto__, __proto__) { }',
        '"use strict"; x = { __proto__: 42, __proto__: 43 }',
        '"use strict"; x = { get __proto__() { }, __proto__: 43 }',
        'var',
        'let',
        'const',
        '{ ;  ;  ',
        'function t() { ;  ;  ',
        'let let',
        'const let=4',
        'for (let let=4;;) {}',
        'for (let in arr) {}',
        'for (let let in arr) {}',
        {
          content: 'class let { }',
          explanation: "Esprima counts comments in its loc, Flow doesn't",
          expected_differences: {
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Unexpected token let',
              actual: 'Use of future reserved word in strict mode'
            },
          }
        },
        'class A { foo() { let let } }',
        {
          content: 'function foo([a.a]) {}',
          explanation: 'Esprima is off by one, flow fails to parse it',
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 18,
              actual: '15-16'
            },
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Invalid left-hand side in formals list',
              actual: 'Unexpected token .',
            }
          },
        },
        {
          content: 'var f = function ([a.a]) {}',
          explanation: 'Esprima is off by one, flow fails to parse it',
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 23,
              actual: '20-21'
            },
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Invalid left-hand side in formals list',
              actual: 'Unexpected token .',
            }
          },
        },
    ],
    'Invalid unicode related syntax': [
        'x\\u005c',
        'x\\u002a',
        'a\\u',
        '\\ua',
        'var x = /[a-z\n]/\\ux',
        '\u203F = 10',
        '\\u005c',
        '\\u0000',
        '\u200C = []',
        '\u200D = []',
    ],
    'JSX': [
        '<a />;\n',
        '<n:a n:v />',
        '<a n:foo="bar"> {value} <b><c /></b></a>',
        '<a b={" "} c=" " d="&amp;" />',
        '<a\n/>',
        /* TODO unicode support
        '<日本語></日本語>',
        */
        '<AbC-def\n  test="&#x0026;&#38;">\nbar\nbaz\n</AbC-def>',
        '<a b={x ? <c /> : <d />} />',
        '<a>{}</a>',
        '<a>{/* this is a comment */}</a>',
        '<div>@test content</div>',
        '<div><br />7x invalid-js-identifier</div>',
        '<a.b></a.b>',
        '<a.b.c></a.b.c>',
        '<div {...props} />',
        '<div {...props} post="attribute" />',
        '<div pre="leading" pre2="attribute" {...props}></div>',
        '<a>    </a>',
        'function() { return <div/>; }'
    ],
    'Invalid JSX Syntax': [
        '</>',
        '<a: />',
        '<:a />',
        '<a b=d />',
        '<a>',
        '<a></b>',
        '<a foo="bar',
        '<a:b></b>',
        '<a:b.c></a:b.c>',
        '<a.b:c></a.b:c>',
        '<a.b.c></a>',
        '<.a></.a>',
        '<a.></a.>',
        '<a[foo]></a[foo]>',
        '<a[\'foo\']></a[\'foo\']>',
        '<a><a />',
        '<a b={}>',
        '<a>{"str";}</a>',
        '<span className="a", id="b" />',
        '<div className"app">',
        '<div {props} />',
        '<div>stuff</div {...props}>',
        '<div {...props}>stuff</div {...props}>',
        '<div><a/><b/><c/>',
    ],
    'Invalid Type Annotations': [
        'function foo(callback:) {}',
        'function foo(): {}',
        'function foo(): { foo() }',
        'function foo(callback:(string) => number) {}',
        'a = {foo(): { return 42; }}',
        'class Foo { get bar<T>() { } }',
        'var a:{a:number b:string}',
    ],

    'ES6 Unicode Code Point Escape Sequence': [
        '"\\u{714E}\\u{8336}"',
        '"\\u{20BB7}\\u{91CE}\\u{5BB6}"'
    ],

    // ECMAScript 6th Syntax, 7.8.3 Numeric Literals

    'ES6: Numeric Literal': [
        '00',
        '0o0',
        'function test() {\'use strict\'; 0o0; }',
        '0o2',
        '0o12',
        '0O0',
        'function test() {\'use strict\'; 0O0; }',
        '0O2',
        '0O12',
        '0b0',
        '0b1',
        '0b10',
        '0B0',
        '0B1',
        '0B10'
    ],

    // ECMAScript 6th Syntax, 11.1. 9 Template Literals

    'ES6 Template Strings': [
        '`42`',
        'raw`42`',
        'raw`hello ${name}`',
        '`$`',
        '`\\n\\r\\b\\v\\t\\f\\\n\\\r\n`',
        '`\n\r\n`',
        '`\\u{000042}\\u0042\\x42\\102\\A`',
        '`Hello\rworld`',
        'new raw`42`',
        '`foo ${\n  "bar"\n} baz`',
        '`foo ${/* a */ "bar" /* b */} baz`',
        '( foo)`bar`'
    ],


    // ECMAScript 6th Syntax, 12.11 The switch statement

    'ES6: Switch Case Declaration': [
        'switch (answer) { case 42: let t = 42; break; }'
    ],

    // ECMAScript 6th Syntax, 13.2 Arrow Function Definitions

    'ES6: Arrow Function': [
        '() => "test"',
        'e => "test"',
        '(e) => "test"',
        '(a, b) => "test"',
        'e => { 42; }',
        'e => ({ property: 42 })',
        // Not an object!
        {
          content: 'e => { label: 42 }',
          explanation: "Esprima counts the implicit semicolon in its loc, " +
            "Flow doesn't",
          expected_differences: {
            'root.body.0.expression.body.body.0.body.loc.end.column': {
              type: 'Wrong number',
              expected: 17,
              actual: 16
            },
            'root.body.0.expression.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 17,
              actual: 16
            },
            'root.body.0.expression.body.body.0.body.range.1': {
              type: 'Wrong number',
              expected: 17,
              actual: 16
            },
            'root.body.0.expression.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 17,
              actual: 16
            },
          }
        },
        '(a, b) => { 42; }',
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
              actual: 'Unexpected token =>',
            }
          },
        },
        '(x=1) => x * x',
        // not strict mode, using eval
        'eval => 42',
        // not strict mode, using arguments
        'arguments => 42',
        // not strict mode, using octals
        '(a) => 00',
        // not strict mode, using eval, IsSimpleParameterList is true
        '(eval, a) => 42',
        // not strict mode, assigning to eval
        '(eval = 10) => 42',
        // not strict mode, using eval, IsSimpleParameterList is false
        '(eval, a = 10) => 42',
        '(x => x)',
        'x => y => 42',
        '(x) => ((y, z) => (x, y, z))',
        'foo(() => {})',
        'foo((x, y) => {})',
        '(sun) => earth',
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


    // ECMAScript 6th Syntax, 13.13 Method Definitions

    'ES6: Method Definition': [
        {
          content: 'x = { method() { } }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 15,
              actual: 12,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 15,
              actual: 12,
            },
          },
        },
        {
          content: 'x = { method(test) { } }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 19,
              actual: 12,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 19,
              actual: 12,
            },
          },
        },
        {
          content: 'x = { \'method\'() { } }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 17,
              actual: 14,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 17,
              actual: 14,
            },
          },
        },
        {
          content: 'x = { get() { } }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 12,
              actual: 9,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 12,
              actual: 9,
            },
          },
        },
        {
          content: 'x = { set() { } }',
          explanation: "Esprima-fb doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.0.expression.right.properties.0.value.range.0': {
              type: 'Wrong number',
              expected: 12,
              actual: 9,
            },
            'root.body.0.expression.right.properties.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 12,
              actual: 9,
            },
          },
        },
    ],

    'Array Comprehension': [
        '[[x,b,c] for (x in []) for (b in []) if (b && c)]' ,
        '[x for (a in [])]' ,
        '[1 for (x in [])]' ,
        '[(x,1) for (x in [])]' ,
        '[x for (x of array)]',
        '[x for (x of array) for (y of array2) if (x === test)]'
    ],

    // http://wiki.ecmascript.org/doku.php?id=harmony:object_literals#object_literal_property_value_shorthand

    'Harmony: Object Literal Property Value Shorthand': [
        'x = { y, z }'
    ],

    // http://wiki.ecmascript.org/doku.php?id=harmony:destructuring

    'Harmony: Destructuring': [
        '[a, b] = [b, a]',
        {
          content: '({ responseText: text }) = res',
          explanation: "Esprima counts the parens in the loc, flow doesn't.",
          expected_differences: {
            'root.body.0.expression.loc.start.column': {
              type: 'Wrong number',
              expected: 0,
              actual: 1
            },
            'root.body.0.expression.range.0': {
              type: 'Wrong number',
              expected: 0,
              actual: 1
            },
          }
        },
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

    // http://wiki.ecmascript.org/doku.php?id=harmony:modules

    'Harmony: Modules': [
        'module "crypto" {}',
        'module crypto from "crypto";',
        'module "crypto/e" {}',
        'export var document',
        'export var document = { }',
        'export let document',
        'export let document = { }',
        'export const document = { }',
        'export function parse() { }',
        'export class Class {}',
        'export default = 42',
        'export *',
        'export * from "crypto"',
        'export { encrypt }',
        'export { encrypt, decrypt }',
        'export { encrypt as default }',
        'export { encrypt, decrypt as dec }',
        'module "lib" { export var document }',
        'module "lib" { export var document = { } }',
        'module "lib" { export let document }',
        'module "lib" { export let document = { } }',
        'module "lib" { export const document = { } }',
        'module "lib" { export function parse() { } }',
        'module "lib" { export class Class {} }',
        'module "lib" { export * }',
        'module "security" { export * from "crypto" }',
        'module "crypto" { export { encrypt } }',
        'module "crypto" { export { encrypt, decrypt } }',
        'module "crypto" { export { encrypt, decrypt as dec } }',
        'import "jquery"',
        'import $ from "jquery"',
        'import { encrypt, decrypt } from "crypto"',
        'import { encrypt as enc } from "crypto"',
        'import { decrypt, encrypt as enc } from "crypto"',
        'import default from "foo"',
        'import { null as nil } from "bar"',
        'module "security" { import "cryto" }',
        'module()',
        'module "foo" { module() }'
    ],


    // http://wiki.ecmascript.org/doku.php?id=harmony:generators

    'Harmony: Yield Expression': [
        {
          content: '(function* () { yield v })',
          explanation: "Esprima counts the parens and implict semicolon in " +
            "its loc. Flow doesn't",
          expected_differences: {
            'root.body.0.expression.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 24,
              actual: 23
            },
            'root.body.0.expression.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 24,
              actual: 23
            },
          }
        },
        {
          content: '(function* () { yield *v })',
          explanation: "Esprima counts the parens and implict semicolon in " +
            "its loc. Flow doesn't",
          expected_differences: {
            'root.body.0.expression.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 25,
              actual: 24
            },
            'root.body.0.expression.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 25,
              actual: 24
            },
          }
        },
        {
          content: 'function* test () { yield *v }',
          explanation: "Esprima counts the implict semicolon in its loc, flow " +
            "doesn't",
          expected_differences: {
            'root.body.0.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 29,
              actual: 28
            },
            'root.body.0.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 29,
              actual: 28
            },
          }
        },
        {
          content: 'var x = { *test () { yield *v } };',
          explanation: "Esprima counts the implict semicolon in its loc, flow " +
            "doesn't. Esprima-fb doesn't include params in onExpression " +
            "location",
          expected_differences: {
            'root.body.0.declarations.0.init.properties.0.value.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 30,
              actual: 29
            },
            'root.body.0.declarations.0.init.properties.0.value.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 30,
              actual: 29
            },
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
        'function* t() {}',
        {
          content: '(function* () { yield yield 10 })',
          explanation: "Esprima counts the parens and implict semicolon in " +
            "its loc. Flow doesn't",
          expected_differences: {
            'root.body.0.expression.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 31,
              actual: 30
            },
            'root.body.0.expression.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 31,
              actual: 30
            },
          }
        },
        'function* f () { var e = () => yield 1; }',
    ],



    // http://wiki.ecmascript.org/doku.php?id=harmony:iterators

    'Harmony: Iterators': [
        'for(x of list) process(x);',
        'for (var x of list) process(x);',
        'for (let x of list) process(x);',
        {
          content: 'for (var x = 42 of list) process(x);',
          explanation: "Esprima thinks this is valid, it isn't",
          expected_differences : {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: undefined,
              actual: 6,
            },
            'root.errors.0.line': {
              type: 'Wrong error line',
              expected: undefined,
              actual: 1,
            },
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: undefined,
              actual: 'Invalid left-hand side in for-of',
            }
          }
        }
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
            "FunctionExpression location",
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
          content: '"use strict"; (class A { static constructor() { super() }})',
          explanation: "Esprima counts the implicit semicolon in its loc, " +
            "Flow doesn't. Esprima-fb also doesn't include params in " +
            "FunctionExpression location",
          expected_differences: {
            'root.body.1.expression.body.body.0.value.body.body.0.loc.end.column': {
              type: 'Wrong number',
              expected: 56,
              actual: 55
            },
            'root.body.1.expression.body.body.0.value.body.body.0.range.1': {
              type: 'Wrong number',
              expected: 56,
              actual: 55
            },
            'root.body.1.expression.body.body.0.value.range.0': {
              type: 'Wrong number',
              expected: 46,
              actual: 43,
            },
            'root.body.1.expression.body.body.0.value.loc.start.column': {
              type: 'Wrong number',
              expected: 46,
              actual: 43,
            },
          }
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

    'Harmony: Invalid Class (strawman)': [
        'class A { get foo() {} get foo() {} }',
        'class A { set foo(v) {} set foo(v) {} }',
        'class A { get foo() {} foo() {} }',
        'class A { foo() {} get foo() {} }',
        'class A { set foo(v) {} foo() {} }',
        'class A { foo() {} set foo(v) {} }',
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
        'x = function(y = 1) {}',
        'function f(a = 1) {}',
        'x = { f: function(a=1) {} }',
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

    // ECMAScript 6th Syntax, 13 - Rest parameters
    // http://wiki.ecmascript.org/doku.php?id=harmony:rest_parameters
    'ES6: Rest parameters': [
        'function f(...a) {}',
        'function f(a, ...b) {}',
        'function f(a = 4, ...b) {}',
        'var a = function f(...a) {}',
        'var a = function f(a, ...b) {}',
        '(...a) => a',
        '(a, ...b) => a',
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

    // https://gist.github.com/sebmarkbage/aa849c7973cb4452c547
    'ES7 Proposal: Rest Properties' : [
        'let {...x} = z',
        'let {x, ...y} = z',
        '(function({x, ...y}) { })'
    ],

    // https://gist.github.com/sebmarkbage/aa849c7973cb4452c547
    'ES7 Proposal: Spread Properties': [
        'let z = {...x}',
        'z = {x, ...y}',
        '({x, ...y, a, ...b, c})',
    ],

    'Harmony Invalid syntax': [
        '0o',
        '0o1a',
        '0o9',
        '0o18',
        '0O',
        '0O1a',
        '0O9',
        '0O18',
        '0b',
        '0b1a',
        '0b9',
        '0b18',
        '0b12',
        '0B',
        '0B1a',
        '0B9',
        '0B18',
        '0B12',
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
        '(function () { yield 10 })',
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
        'function f(a, ...b, c)',
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
        'class Foo { [1 + 1]: string; }',
        'class Foo { 123:string; }',
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
        'class Foo { "prop1":string; }',
        'class Foo { [prop1]: string; }' ,
        'class Foo { prop1:string; prop2:number; }',
        'class Foo { static prop1:string; prop2:number; }',
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
        'function foo({x}: { x: string; }) {}',
        'interface Array<T> { concat(...items: Array<Array<T> | T>): Array<T>; }',
        'interface Array<T> { concat(...items: Array<Array<T> & T>): Array<T>; }',
        'var a: { add(...rest:Array<number>): number; }',
        'var a: { add(x: number, y:number): number; }',
        'var a:Array<number> = [1, 2, 3]',
        'var a: { foo<T>(x: T): number; }',
        'var a: ?{numVal: number};',
        'var a: {numVal: number};',
        'var a: {numVal: number; [indexer: string]: number};',
        'var a: {numVal: number; [index: number]: string};',
        'var a: {numVal: number; strVal: string}',
        'var a: {param1: number; param2: string}',
        'var a: {param1: number; param2?: string}',
        'var a:(...rest:Array<number>) => number',
        'var a: {subObj: ?{strVal: string}}',
        'var a: {subObj: {strVal: string}}',
        'var bar: (str:number, i:number)=> string = foo;',
        'var numVal:number;',
        'var numVal:number = otherNumVal;',
        'var [x]: Array<string> = [ "hello" ];',
        'var x : number | string = 4;',
        'var x : number & string = 4;',
        'var x : () => number | () => string = fn;',
        'var x : () => number & () => string = fn;',
        'var x: typeof Y | number = Y;',
        'var x: typeof Y & number = Y;',
        'var x: typeof Y = Y;',
        'var {x}: {x: string; } = { x: "hello" };',
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
      'interface A { foo: number; }',
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
      'var x: typeof A[];',
    ],
    'Export': {
      'options': { sourceType: "module" },
      'tests': [
        'export * from "foo";',
        'export * from "foo"',
        /* This should be supported...
        'export {} from "foo";',
        */
        'export { bar } from "foo";',
        'export { bar } from "foo"',
        'export { bar, baz } from "foo";',
        'export { bar };',
        /* Esprima should support trailing comma
        'export { bar, }',
        */
        'export { bar, baz };',
        'export var x, y',
        'export var y = 12',
        'export let x, y',
        'export let y = 12',
        'export const x, y',
        'export const y = 12',
        'export function foo() {}',
        'export class A {}',
        'export default 1 + 1;',
        'export default 1 + 1',
        /* Esprima parses default exports wrong
        'export default function foo() {}',
        'export default function *foo() {}',
        'export default class {};',
        'export default class A {};',
        */
      ]
    },
    'Invalid Exports': {
      'options': { sourceType: "module" },
      'tests': [
        /* Esprima doesn't parse nameless exported classes yet
        'export class {}',
        */
        'export function {}',
        'export default function() {}',
        /* Esprima parses default exports wrong
        'export default class A {}',
        */
      ],
    },
    'Import': {
      'options': { sourceType: "module" },
      'tests': [
        'import "MyModule";',
        'import defaultbinding from "MyModule";',
        'import * as namespace from "MyModule";',
        'import {} from "MyModule";',
        'import defaultbinding, {} from "MyModule";',
        /* TODO Esprima should support these
        'import {x,} from "MyModule";',
        'import defaultbinding, {x,} from "MyModule";',
        */
        'import {x} from "MyModule";',
        'import {x,y} from "MyModule";',
        'import {x as z} from "MyModule";',
        'import {x, y as z} from "MyModule";',
        'import defaultbinding, * as namespace from "MyModule";',
        'import defaultbinding, {x} from "MyModule";',
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
        'import type * as namespace from "MyModule";',
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
      'declare var foo',
      'declare var foo;',
      'declare var foo: number;',
      'declare function foo(): void',
      'declare function foo(): void;',
      'declare function foo<T>(): void;',
      'declare function foo(x: number, y: string): void;',
      'declare class A {}',
      'declare class A<T> extends B<T> { x: number }',
      'declare class A { static foo(): number; static x : string }',
      'declare class A { static [ indexer: number]: string }',
      'declare class A { static () : number }',
    ],
    'Invalid Declare Statements': [
      'declare class A { "static" foo(): number }',
      'declare class A { static : number }',
      'declare function foo();',
      'declare function foo(x): void',
    ],
    'Declare Module': [
      'declare module A {}',
      'declare module "./a/b.js" {}',
      'declare module A { declare var x: number; }',
      'declare module A { declare function foo(): number; }',
      'declare module A { declare class B { foo(): number; } }',
    ],
    'Invalid Declare Module': [
      'declare Module A {}',
      'declare module {}',
      '"use strict"; declare module "\\01" {}',
      {
        content: 'declare module A { declare module B {} }',
        explanation: "We realize the error as soon as we see the B",
        expected_differences: {
          'root.errors.0.column': {
            type: 'Wrong error column',
            expected: 19,
            actual: '34-35'
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
              actual: 'Unexpected token export',
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
        content: 'var a: number & (string | bool)',
        explanation: 'Esprima counts the parens in the type locs',
        expected_differences: {
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.range.1': {
            type: 'Wrong number',
            expected: 31,
            actual: 30
          },
          'root.body.0.declarations.0.id.typeAnnotation.typeAnnotation.loc.end.column': {
            type: 'Wrong number',
            expected: 31,
            actual: 30,
          }
        }
      },
      'var a: (typeof A)',
      'var a: Array<(number)>',
      'var a: ([]) = []',
      'var a: (A)',
      'var a: (A.B)',
      'var a: (A<T>)',
      'var a: (A | B)',
      'var a: (A & B)',
    ],
    'Typecasts': [
      '(xxx: number)',
      '({xxx: 0, yyy: "hey"}: {xxx: number; yyy: string})',
      // distinguish between function type params and typecasts
      '((xxx) => xxx + 1: (xxx: number) => number)',
      // parens disambiguate groups from casts
      {
        content: '((xxx: number), (yyy: string))',
        explanation:  'Esprima counts the parens in its locs',
        expected_differences: {
          'root.body.0.expression.range.0': {
            type: 'Wrong number',
            expected: 1,
            actual: 2
          },
          'root.body.0.expression.range.1': {
            type: 'Wrong number',
            expected: 29,
            actual: 28
          },
          'root.body.0.expression.loc.start.column': {
            type: 'Wrong number',
            expected: 1,
            actual: 2
          },
          'root.body.0.expression.loc.end.column': {
            type: 'Wrong number',
            expected: 29,
            actual: 28
          },
        }
      },
    ],
    'Invalid Typecasts': [
      // Must be parenthesized
      'var x: number = 0: number;',
      // ...even within groups
      '(xxx: number, yyy: string)'
    ],
    'Bounded Polymorphism': [
      'function foo<T: Foo>() {}',
      'class Foo<T: Bar>() {}',
    ],
    'For Of Loops': [
        'for(x of list) process(x);',
        'for (var x of list) process(x);',
        'for (let x of list) process(x);',
    ],
    'Invalid For Of Loops': [
        {
          content: 'for (let x = 42 of list) process(x);',
          explanation: 'Exprima is off by one here location-wise '+
            'and I like my error here better',
          expected_differences: {
            'root.errors.0.column': {
              type: 'Wrong error column',
              expected: 16,
              actual: '5-15',
            } ,
            'root.errors.0.message': {
              type: 'Wrong error message',
              expected: 'Unexpected identifier',
              actual: 'Invalid left-hand side in for-of',
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
        'async function foo() { return function await() { }; }',
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
        'async function foo() { var await = 4; }',
        'var x = async function bar() { await foo; }',
        'async function foo() { return await; }',
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
