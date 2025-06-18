// @flow

import {assertExpected} from './exports';

export type ExpectedLocal1 = {
    foo?: number,
    bar: 'foo' | 'bar',
}

export type ExpectedLocal2 = {
    foo?: boolean,
    bar: 'bar' | 'baz',
}

declare export function assertExpected1(expected: ExpectedLocal1): void;
declare export function assertExpected2(expected: ExpectedLocal2): void;
declare export function assertExpected3(expected: {
    foo?: number,
    bar: 'foo' | 'bar',
}): void;

const obj1 = {bar: 'foo' as const}; // annot and import
const obj2 = {bar: 'bar' as const}; // no annot, since expected type is ExpectedLocal1 & ExpectedLocal2
const obj3 = {bar: 'foo' as const}; // annot
const obj4 = {bar: 'foo' as const}; // no annot. Too big

assertExpected(obj1);
assertExpected1(obj2);
assertExpected2(obj2);
assertExpected1(obj3);
assertExpected3(obj4);
