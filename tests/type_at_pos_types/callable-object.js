// @flow

declare opaque type O1;
declare opaque type O2;
declare opaque type O3;
declare opaque type O4;

type Obj1 = {
//     ^?
  (...args: ReadonlyArray<unknown>): void,
  f: <T>(x: T) => T
, ...};

type Obj2 = {
//     ^?
  $call: (...args: ReadonlyArray<unknown>) => void, // named prop
  f: <T>(x: T) => T
, ...};

type Obj3 = {
//     ^?
  [[call]]: (...args: ReadonlyArray<unknown>) => void,
  f: <T>(x: T) => T
, ...};

type Obj4 = {
//     ^?
  (...args: ReadonlyArray<unknown>): O1,
  (...args: ReadonlyArray<unknown>): O2,
  $call: (...args: ReadonlyArray<unknown>) => O3, // named prop
  [[call]]: (...args: ReadonlyArray<unknown>) => O4,
  f: <T>(x: T) => T
, ...};

type Obj5 = {
//     ^?
  [[call]]: (...args: ReadonlyArray<unknown>) => O4,
  $call: (...args: ReadonlyArray<unknown>) => O3, // named prop
  (...args: ReadonlyArray<unknown>): O1,
  (...args: ReadonlyArray<unknown>): O2,
  f: <T>(x: T) => T,
 ...};
