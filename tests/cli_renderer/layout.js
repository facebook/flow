/**
 * @format
 * @flow
 */

{
  const o = {a: {b: {c: {d: 42}}}};
  [o.a.b.c.d, o.a.b.c, o.a.b, o.a, o] as [empty, empty, empty, empty, empty];
  [o, o.a, o.a.b, o.a.b.c, o.a.b.c.d] as [empty, empty, empty, empty, empty];
  [o.a.b.c.d, o, o.a.b.c, o.a, o.a.b] as [empty, empty, empty, empty, empty];
  [o, o.a.b.c.d, o.a, o.a.b.c, o.a.b] as [empty, empty, empty, empty, empty];
  [o.a.b, o, o.a.b.c, o.a, o.a.b.c.d] as [empty, empty, empty, empty, empty];
  [o.a.b, o.a.b.c.d, o.a.b.c, o.a, o] as [empty, empty, empty, empty, empty];
}

{
  const o = {a: 1, b: 2};
  [o, o.a, o.b] as [empty, empty, empty];
  [o.a, o, o.b] as [empty, empty, empty];
  [o.a, o.b, o] as [empty, empty, empty];
  [o, o.b, o.a] as [empty, empty, empty];
  [o.b, o, o.a] as [empty, empty, empty];
  [o.b, o.a, o] as [empty, empty, empty];
}
