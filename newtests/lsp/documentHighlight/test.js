/**
 * @flow
 * @format
 */

import type {Suite} from 'flow-dev-tools/src/test/Suite';
const {readFileSync, readdirSync} = require('fs');
const {join} = require('path');
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(
  ({
    lspNotification,
    lspStartAndConnect,
    lspRequestAndWaitUntilResponse,
    addFiles,
  }) => {
    function snapshot(fixture, line, col, expectedFile) {
      return lspRequestAndWaitUntilResponse('textDocument/documentHighlight', {
        textDocument: {
          uri: `<PLACEHOLDER_PROJECT_URL>/__fixtures__/${fixture}`,
        },
        position: {line: line, character: col},
      }).verifyLSPMessageSnapshot(
        join(__dirname, '__snapshots__', expectedFile),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      );
    }
    const fixtures = readdirSync(join(__dirname, '__fixtures__')).map(file =>
      join('__fixtures__', file),
    );
    return [
      test('Variable defs and uses', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshot('locals.js', 3, 5, 'var_defs_1.json'),
        snapshot('locals.js', 4, 2, 'var_defs_2.json'),
      ]),
      test('Nested functions', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshot('locals.js', 9, 10, 'nested_funcs_1.json'),
        snapshot('locals.js', 12, 3, 'nested_funcs_2.json'),
      ]),
      test('Classes', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshot('locals.js', 17, 7, 'classes_1.json'),
      ]),
      // test('Type aliases', [
      //   addFiles(...fixtures),
      //   lspStartAndConnect(),
      //   snapshot('locals.js', 22, 6, 'type_aliases_1.json'),
      // ]),
      test('Refinements', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshot('locals.js', 27, 6, 'refinements_1.json'),
        snapshot('locals.js', 28, 6, 'refinements_2.json'),
        snapshot('locals.js', 29, 16, 'refinements_3.json'),
        snapshot('locals.js', 30, 8, 'refinements_4.json'),
        snapshot('locals.js', 32, 2, 'refinements_5.json'),
      ]),
      test('Destructuring', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshot('locals.js', 35, 7, 'destructuring_1.json'),
        snapshot('locals.js', 36, 10, 'destructuring_2.json'),
        snapshot('locals.js', 36, 26, 'destructuring_3.json'),
        snapshot('locals.js', 37, 7, 'destructuring_4.json'),
      ]),
      // test('Not in scope', [
      //   addFiles(...fixtures),
      //   lspStartAndConnect(),
      //   snapshot('locals.js', 40, 2, 'not_in_scope_1.json'),
      //   snapshot('locals.js', 41, 2, 'not_in_scope2.json'),
      //   snapshot('locals.js', 41, 9, 'not_in_scope3.json'),
      //   snapshot('locals.js', 42, 2, 'not_in_scope4.json'),
      // ]),
      test('JSX', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        // snapshot('locals.js', 49, 4, 'jsx_1.json'),
        // Use as a JSX component class
        snapshot('jsx.js', 4, 7, 'jsx_2.json'),
      ]),
      test('Imports', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshot('locals.js', 56, 2, 'imports_1.json'),
        // This is a type, which doesn't work yet
        // snapshot('locals.js', 54, 9, 'imports_2.json'),
      ]),
      // test('Qualified types', [
      //   addFiles(...fixtures),
      //   lspStartAndConnect(),
      //   snapshot('locals.js', 57, 9, 'qualified_types_1.json'),
      // ]),
      test('Exports', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshot('locals.js', 65, 20, 'exports_1.json'),
        snapshot('locals.js', 66, 6, 'exports_2.json'),
      ]),
      test('Methods and properties', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        // Method declaration
        snapshot('locals.js', 71, 3, 'methods_1.json'),
        // Property declaration
        snapshot('locals.js', 72, 5, 'methods_2.json'),
        // Method call (finds definition and other references)
        snapshot('locals.js', 81, 13, 'methods_3.json'),
        // Method call on an imported class (finds other references but not the definition since it's in another file)
        snapshot('locals.js', 91, 16, 'methods_4.json'),
        // Method call within a class that has type params
        snapshot('locals.js', 97, 10, 'methods_5.json'),
      ]),
      test('Class inheritance', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        // Instance method on a superclass
        snapshot('classInheritance.js', 4, 3, 'class_inheritance_1.json'),
        // Call of instance method on subclass which does not override
        snapshot('classInheritance.js', 20, 10, 'class_inheritance_2.json'),
        // Instance method on a subclass which does override
        snapshot('classInheritance.js', 10, 3, 'class_inheritance_3.json'),
        // Call of instance method on a subclass which does override
        snapshot('classInheritance.js', 21, 10, 'class_inheritance_4.json'),
        // Definition of a method in a parameterized class
        snapshot('classInheritance.js', 25, 3, 'class_inheritance_5.json'),
        // Call of an instance method on an upcasted class
        snapshot('classInheritance.js', 31, 15, 'class_inheritance_6.json'),

        // TODO it would be nice if the results included the use of the inherited class property.
        // Definition of a static class property
        snapshot('classInheritance.js', 3, 10, 'class_inheritance_7.json'),
        // TODO it would be nice if the results included the use of the inherited class property.
        // Use of a static class property
        snapshot('classInheritance.js', 37, 3, 'class_inheritance_8.json'),
        // TODO it would be nice if this returned results.
        // Use of an inherited static class property
        snapshot('classInheritance.js', 38, 3, 'class_inheritance_9.json'),
      ]),
      test('Objects', [
        addFiles(...fixtures),
        lspStartAndConnect(),

        // Method declaration in an object type alias
        snapshot('objects.js', 3, 4, 'objects_1.json'),
        // Property declaration in an object type alias
        snapshot('objects.js', 4, 4, 'objects_2.json'),
        // Method call on an object type alias
        snapshot('objects.js', 8, 4, 'objects_3.json'),

        // Property access on an object without an annotation
        snapshot('objects.js', 14, 4, 'objects_4.json'),
        // Property definition on an object without an annotation
        snapshot('objects.js', 13, 12, 'objects_5.json'),

        // Introduction of a shadow property via a write
        snapshot('objects.js', 18, 4, 'objects_6.json'),
        // Use of a shadow property
        snapshot('objects.js', 19, 4, 'objects_7.json'),

        // Introduction of a shadow property via a read
        snapshot('objects.js', 22, 4, 'objects_8.json'),
        // Write of a shadow property introduced via a read
        snapshot('objects.js', 23, 4, 'objects_9.json'),

        // Introduction of a shadow property that is never written
        snapshot('objects.js', 26, 4, 'objects_10.json'),
        // Read of a shadow property that is never written
        snapshot('objects.js', 27, 4, 'objects_11.json'),

        // Use of a property that came through type spread
        snapshot('objects.js', 34, 25, 'objects_12.json'),

        // Object literal method
        snapshot('objects.js', 7, 21, 'objects_13.json'),

        // Object literal property
        snapshot('objects.js', 7, 31, 'objects_14.json'),

        // Method in object literal which flows into two types
        snapshot('objects.js', 41, 13, 'objects_15.json'),

        // Property in object literal which flows into two types
        snapshot('objects.js', 41, 23, 'objects_16.json'),
      ]),
      test('Declare', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshot('declare.js', 2, 13, 'declare_1.json'),
      ]),
      test('Import Star', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        // object created via `import *`
        snapshot('importStar.js', 4, 10, 'importStar_1.json'),
      ]),
      test('Unchecked', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        // Property in an unchecked file
        snapshot('unchecked.js', 3, 5, 'unchecked_1.json'),
      ]),
      test('Optional chaining', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshot('optional-chaining.js', 16, 6, 'optionalChaining_1.json'),
        snapshot('optional-chaining.js', 16, 10, 'optionalChaining_2.json'),
      ]),
      test('Idx', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshot('idx.js', 5, 4, 'idx_1.json'),
        snapshot('idx.js', 11, 25, 'idx_2.json'),
        snapshot('idx.js', 6, 6, 'idx_3.json'),
        snapshot('idx.js', 11, 29, 'idx_4.json'),
      ]),
      test('Unions', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshot('unions.js', 6, 3, 'unions_1.json'),
        snapshot('unions.js', 9, 30, 'unions_2.json'),
        snapshot('unions.js', 16, 10, 'unions_3.json'),
      ]),
      test('Unsaved', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        lspNotification('textDocument/didOpen', {
          textDocument: {
            uri: '<PLACEHOLDER_PROJECT_URL>/__fixtures__/empty.js',
            languageId: 'javascript',
            version: 1,
            text: `// @flow`,
          },
        }).verifyAllLSPMessagesInStep(
          [],
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
        lspNotification('textDocument/didChange', {
          textDocument: {
            uri: '<PLACEHOLDER_PROJECT_URL>/__fixtures__/empty.js',
            version: 2,
          },
          contentChanges: [
            {
              text: `// @flow

type Bar = { foo: string };

const x: Bar = { foo: 'asdf' };
x.foo;
`,
            },
          ],
        }).verifyAllLSPMessagesInStep(
          [],
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
        snapshot('empty.js', 2, 15, 'unsaved_1.json'),
      ]),
    ];
  },
): Suite);
