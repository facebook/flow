//@flow

type T = {a: null | number};
declare var t: ?T;
if (t?.a === null) {
  t as T; // yes
  t.a as null; // yes
} else {
  t as null | void; // no
  t.a as number; // no for two reasons
}
if (t?.a !== null) {
  t as null | void; // no
  t.a as number; // no for two reasons
} else {
  t as T; // yes
  t.a as null; // yes
}

type S = {a: ?number};
declare var s: ?S;
if (s?.a == null) {
  s as S; // no, may be nulled
  s.a as null | void; // no for two reasons
} else {
  s as S; // yes
  s.a as number; // yes: s cannot be null and s.a cannot be null or void
}
if (s?.a != null) {
  s as S; // yes
  s.a as number; // yes: s cannot be null and s.a cannot be null or void
} else {
  s as S; // no, may be nulled
  s.a as null | void; // no for two reasons
}
if (s?.a === undefined) {
  s as S; // no, may be nulled
  s.a as null | void; // no for two reasons
} else {
  s as S; // yes
  s.a as number; // no: s.a can be null
  s.a as number | null; //yes
}
if (s?.a == undefined) {
  s as S; // no, may be nulled
  s.a as null | void; // no for two reasons
} else {
  s as S; // yes
  s.a as number; // yes: s cannot be null and s.a cannot be null or void
}

type W = {a: number};
declare var w: ?W;

if (w?.a === 42) {
  w as W;
  w.a as 42;
} else {
  w as W; // no
  w.a; // no
}

declare var a: ?{b: number | string};
if (typeof a?.b === 'number') {
  a.b as number;
  a as {};
} else {
  a.b; // nope
  a as null | void; // nope
}

declare var b: ?{a?: number};
if (typeof b?.a === 'undefined') {
  b.a; //nope
  b as null | void; // nope
} else {
  b as {};
  b.a as number;
}

if (b?.a instanceof Object) {
  b.a as number;
  b as {};
} else {
  b.a; // nope
  b as null | void; // nope
}

declare var c: ?{d?: Array<number>};
if (Array.isArray(c?.d)) {
  c.d[0] as number;
  c as {};
} else {
  c.d; //nope
  c as null | void; //nope
}

// should not cause refinement with current model
declare var b1: ?{a?: number};
declare var c1: number;
if (b1?.a === c1) {
  b1.a as number; // b1.a may not exist and may not be number
}
