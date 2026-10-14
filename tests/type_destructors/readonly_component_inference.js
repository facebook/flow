import * as React from 'react';

type Box<T> = Readonly<{value: T}>;
type Props<T> = Readonly<{
  render: component(values: ReadonlyArray<Box<T>>),
  values: ReadonlyArray<Box<T>>,
}>;
component Wrapper<T>(...props: Props<T>) {
  return null;
}
component Wide(values: ReadonlyArray<Box<number | string>>) {
  return null;
}
component Generic<T>(values: ReadonlyArray<Box<T>>) {
  return null;
}
declare function makeBoxes<T>(value: () => T): ReadonlyArray<Box<T>>;
declare function getNumber(): number;
const values = makeBoxes(getNumber);
declare const flag: boolean;
<Wrapper render={flag ? Wide : Generic} values={values} />; // TODO: Error: `string` in `render.values` is incompatible with `number` from `values`.
