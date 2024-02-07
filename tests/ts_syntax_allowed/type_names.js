{
  const a: ReadonlyArray<number> = [3]; // ok
  a.push(4); // error: readonly
  const b: ReadonlySet<number> = new Set([3]); // ok
  b.add(3); // error: readonly
  const c: ReadonlyMap<number, string> = new Map([[3, '']]); // ok
  c.add(3, ''); // error: readonly
  let d: NonNullable<string | null> = ''; // ok
  d = null; // error: nonnull
  const e: Readonly<{foo: string}> = {foo: ''}; // ok
  e.foo = ''; // error: readonly
}

{
  const exported_with_ts_typenames = require('./exported');
  exported_with_ts_typenames.a.push(4); // error: readonly
  exported_with_ts_typenames.b.add(3); // error: readonly
  exported_with_ts_typenames.c.add(3, ''); // error: readonly
  exported_with_ts_typenames.d = null; // error: nonnull
  exported_with_ts_typenames.e.foo = ''; // error: readonly
}
