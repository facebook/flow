// @flow

import {
    obj1, // {+f: "a"}
//  ^
    obj2, // {+f:{g:string}}
//  ^
    obj3, // {+f: "a"}
//  ^
    obj4, // {+f: "a"}
//  ^
    obj5, // {+a: "a", +b: "b", +c: {d: string}, +e: "e"}
//  ^
    obj6, // {prop: {+f: "a"}}
//  ^
} from './export_const';

import defaultObj from './export_default'; // {+f: "a"}
//     ^

import moduleExports from './module_exports'; // {+f: "a"}
//     ^
import {
    obj7,
//  ^
    obj8,
//  ^
} from './export_const'; // {+f: string}
