// @flow

declare opaque type O1;
declare opaque type O2;
declare opaque type O3;
declare opaque type O4;

type Obj1 = {
//     ^?
  (...args: $ReadOnlyArray<unknown>): void,
  f: <T>(x: T) => T
, ...};

type Obj2 = {
//     ^?
  $call: (...args: $ReadOnlyArray<unknown>) => void, // named prop
  f: <T>(x: T) => T
, ...};

type Obj3 = {
//     ^?
  [[call]]: (...args: $ReadOnlyArray<unknown>) => void,
  f: <T>(x: T) => T
, ...};

type Obj4 = {
//     ^?
  (...args: $ReadOnlyArray<unknown>): O1,
  (...args: $ReadOnlyArray<unknown>): O2,
  $call: (...args: $ReadOnlyArray<unknown>) => O3, // named prop
  [[call]]: (...args: $ReadOnlyArray<unknown>) => O4,
  f: <T>(x: T) => T
, ...};

type Obj5 = {
//     ^?
  [[call]]: (...args: $ReadOnlyArray<unknown>) => O4,
  $call: (...args: $ReadOnlyArray<unknown>) => O3, // named prop
  (...args: $ReadOnlyArray<unknown>): O1,
  (...args: $ReadOnlyArray<unknown>): O2,
  f: <T>(x: T) => T,
 ...};
