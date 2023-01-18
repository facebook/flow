// @flow

import * as React from 'react';

const Enum = Object.freeze({A:'a', B: 'b'});

type A = { __type__: 'a', arr: Array<string> } | { __type__: 'b', arr: Array<number> };
type F = { __type__: 'a', f: (v: string) => string } | { __type__: 'b', f: (v: number) => number };
declare function CompAProp(A): React.MixedElement;
declare function CompFProp(F): React.MixedElement;

// OK

const a1: A = { __type__: 'a', arr: [] }; // ok
const a2: A = { arr: [], __type__: 'a' }; // ok
const a3: A = { __type__: 'b', arr: [] }; // ok
const a4: A = { arr: [], __type__: 'b' }; // ok
const a5: A = { __type__: Enum.A, arr: [] }; // ok
const a6: A = { arr: [], __type__: Enum.A }; // ok
const a7: A = { __type__: Enum.B, arr: [] }; // ok
const a8: A = { arr: [], __type__: Enum.B }; // ok

const f1: F = { __type__: 'a', f: (v) => (v: string) }; // ok
const f2: F = { f: (v) => (v: string), __type__: 'a' }; // ok
const f3: F = { __type__: 'b', f: (v) => (v: number) }; // ok
const f4: F = { f: (v) => (v: number), __type__: 'b' }; // ok
const f5: F = { __type__: Enum.A, f: (v) => (v: string) }; // ok
const f6: F = { f: (v) => (v: string), __type__: Enum.A }; // ok
const f7: F = { __type__: Enum.B, f: (v) => (v: number) }; // ok
const f8: F = { f: (v) => (v: number), __type__: Enum.B }; // ok

<CompAProp __type__="a" arr={[]} />; // ok
<CompAProp arr={[]} __type__="a" />; // ok
<CompAProp __type__="b" arr={[]} />; // ok
<CompAProp arr={[]} __type__="b" />; // ok
<CompAProp __type__={Enum.A} arr={[]} />; // ok
<CompAProp arr={[]} __type__={Enum.A} />; // ok
<CompAProp __type__={Enum.B} arr={[]} />; // ok
<CompAProp arr={[]} __type__={Enum.B} />; // ok
<CompFProp __type__="a" f={(v) => (v: string)} />; // ok
<CompFProp f={(v) => (v: string)} __type__="a" />; // ok
<CompFProp __type__="b" f={(v) => (v: number)} />; // ok
<CompFProp f={(v) => (v: number)} __type__="b" />; // ok
<CompFProp __type__={Enum.A} f={(v) => (v: string)} />; // ok
<CompFProp f={(v) => (v: string)} __type__={Enum.A} />; // ok
<CompFProp __type__={Enum.B} f={(v) => (v: number)} />; // ok
<CompFProp f={(v) => (v: number)} __type__={Enum.B} />; // ok

// Errors
declare function a(): 'a';
const e1: A = { __type__: a(), arr: [] }; // // err
const e2: F = { __type__: a(), f: (v) => (v: string) }; // error
<CompAProp __type__={a()} arr={[]} />; // // err
<CompFProp __type__={a()} f={(v) => (v: string)} />; // error

function numeric_sentinel() {
  type T = { n: 1 | 2, fn: (x: number) => void };
  const t: T = { n: 1, fn: (x) => {} }; // ok, sentinel `1` should match part of above union
}
