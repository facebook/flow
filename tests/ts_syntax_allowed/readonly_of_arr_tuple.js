declare const foo: readonly [string, number];
declare const bar: readonly string[];
declare const baz: readonly {foo: string}; // error
foo[0] as number; // error: string ~> number
foo[0] = ''; // error
bar[0] as number; // error: string ~> number
bar[0] = ''; // error
baz.sadfasdfd; // ok: baz is any

import {readonly_tuple, readonly_array} from './exported';
readonly_tuple[0] as number; // error: string ~> number
readonly_tuple[0] = ''; // error
readonly_array[0] as number; // error: string ~> number
readonly_array[0] = ''; // error
