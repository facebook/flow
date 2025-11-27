import type {ReadonlyR} from './exported';

declare const e: empty;
declare const u: unknown;

e as ReadonlyR;
u as ReadonlyR;

import {RNamed} from './exported';
{
  const x = RNamed {a: 1}; // OK
  x as RNamed; // OK
  x as empty; // ERROR

  RNamed {a: 1, b: 's'}; // OK
  RNamed {}; // ERROR
  RNamed {a: 1, b: 's', xxx: true}; // ERROR
  RNamed {a: false}; // ERROR
}

import RDefault from './exported-default';
{
  const x = RDefault {a: 1}; // OK
  x as RDefault; // OK
  x as empty; // ERROR

  RDefault {a: 1, b: 's'}; // OK
  RDefault {}; // ERROR
  RDefault {a: 1, b: 's', xxx: true}; // ERROR
  RDefault {a: false}; // ERROR
}
