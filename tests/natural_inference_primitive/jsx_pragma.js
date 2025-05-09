// @jsx createElement

declare export function createElement<TProps: {...}>(
  Component: TProps => void,
  props: NoInfer<?TProps>,
): $ReadOnly<{
  props: {[propName: string]: mixed, ...},
}>;

declare var C: (props: $ReadOnly<{foo: Array<{bar: 'BAZ'}>}>) => void;
const c1 = <C foo={[{bar: 'BAZ'}]} />; // okay
const c2 = <C foo={[{bar: 'BAK'}]} />; // error 'BAZ' != 'BAK'
const c3 = <C {...{foo: [{bar: 'BAZ'}]}} />; // okay

declare var D: (props: $ReadOnly<{foo: Array<{bar: string}>}>) => void;
const d1 = <D foo={[{bar: 'BAZ'}]} />; // okay
const d2 = <D foo={[{bar: 'BAK'}]} />; // okay
