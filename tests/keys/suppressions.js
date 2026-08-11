/**
 * @format
 * @flow
 */

type StrDict = {[key: string]: unknown};
type ObjLit = {foo: string};
declare class Cls {
  m: string;
}

declare const n: number;
declare const s: string;

// A lower bound that cannot spell a property name is not a property the operand
// is missing, it is not a key at all. Filing it as a lookup failure would move
// it to `prop-missing` and silently unsuppress every
// `$FlowFixMe[incompatible-type]` sitting on one of these today.
// $FlowFixMe[incompatible-type]
n as keyof StrDict; // No error, the suppression applies
// $FlowFixMe[incompatible-type]
n as keyof ObjLit; // No error, the suppression applies
// $FlowFixMe[incompatible-type]
n as keyof Cls; // No error, the suppression applies
// $FlowFixMe[incompatible-type]
n as keyof (StrDict | ObjLit); // No error, the suppression applies

// A string that could have been a key stays a `prop-missing` lookup failure.
s as keyof ObjLit; // Error: prop-missing
s as keyof Cls; // Error: prop-missing
