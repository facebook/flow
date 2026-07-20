// The name-munging transform (babel-plugin-class-private-props) only rewrites
// non-computed identifier keys, so only `_ident` is hidden from the exported
// interface. Every computed key stays public at runtime, even when it resolves
// to an underscore-prefixed name, so `_prop`, `_meth`, and `_ref` all remain
// accessible to consumers.
const p = '_ref' as const;

export class C {
  _ident: number = 1;
  ['_prop']: number = 2;
  ['_meth'](): number { return 3; }
  [p]: number = 4;
  ['kept']: number = 5;
}
