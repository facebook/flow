import {
  getProp,
  getElem,
  spread,
  exactProp,
  proto,
  not,
  keyMirrorProp,
  keysLength,
  nonHomomorphicProp,
  optionalProp,
} from './export_values';

getProp as string; // OK
getProp as number; // ERROR
// Computed access in an export position is `any`, as it is for object types.
getElem as empty; // OK
spread.f as string; // OK
spread.f as number; // ERROR
exactProp as number; // ERROR
proto.f as number; // ERROR
not as boolean; // OK
not as string; // ERROR
keyMirrorProp as 'f'; // OK
keyMirrorProp as 'g'; // ERROR
keysLength as string; // ERROR
nonHomomorphicProp as number; // OK
nonHomomorphicProp as string; // ERROR
optionalProp as string | void; // OK
optionalProp as string; // ERROR
