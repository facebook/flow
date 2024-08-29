// @flow

import {
    obj1, // TODO {+f: "a"}
//  ^
    obj2, // TODO {+f:{g:string}}
//  ^
    obj3, // TODO {+f: "a"}
//  ^
    obj4, // TODO {+f: "a"}
//  ^
    obj5, // TODO {+a: "a", +b: "b", +c: {d: string}, +e: "e"}
//  ^
    obj6, // TODO {prop: {+f: "a"}}
//  ^
} from './export_const';

import defaultObj from './export_default'; // TODO {+f: "a"}
//     ^

import moduleExports from './module_exports'; // TODO {+f: "a"}
//     ^
