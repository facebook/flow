/* @flow */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, flowCmd}) => [
  test('class exports', [
    addFile('named_class_exports.js'),
    flowCmd(['shadow-file', 'named_class_exports.js']).stdout(`
      declare class Class0 {
      }
      declare export class Base<A, B, C> {
        baseInst: Base<number, string, mixed>;
        childInst: Child<string, number>;

        baseMethod(a: number, b: string): number;
        overriddenMethod(a: {b: number, c: number}): number;
      }
      declare export class Child<A, B> extends Base<A, B, mixed> {
        notExported: Class0;

        overriddenMethod(a: {b: number}): number;
      }
    `)
    .stderr('')
  ]),

  test('named variable exports', [
    addFile('named_variable_exports.js'),
    flowCmd(['shadow-file', 'named_variable_exports.js']).stderr('').stdout(`
      declare class Class0 {
      }
      declare export var constExport: 42;
      declare export var letExport: 43;
      export type typeExport = number;
      declare export var unexportedAlias: string;
      declare export var unexportedNominal: Class0;
      declare export var varExport: 44;
    `)
  ]),

  test('function exports', [
    addFile('function_exports.js'),
    flowCmd(['shadow-file', 'function_exports.js']).stderr('').stdout(`
      declare export function mono(a: number, b: {c: number}): number;
      declare export function poly<T: number, U: T, V: U>(a: V): number;
    `)
  ]),

  test('non-@flow files', [
    addFile('non_flow_file.js'),
    flowCmd(['shadow-file', 'non_flow_file.js']).stderr('').stdout(`
      declare export function addNum(a: number, b: number): number;
    `)
  ]),

  test('type errors halt and stderr', [
    addFile('type_error.js'),
    flowCmd(['shadow-file', 'type_error.js']).stdout('').stderr(`
      type_error.js:3
        3: export var a: string = 42;
                                  ^^ number. This type is incompatible with
        3: export var a: string = 42;
                         ^^^^^^ string
      Found 1 error
      There must be no type errors in order to generate a shadow file!
    `)
  ]),
]);
