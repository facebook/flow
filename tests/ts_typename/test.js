{
  const a: ReadonlyArray<number> = [3]; // error: ts-syntax
  a.push(4); // currently no error because a is any, inconsistent with type-sig behavior
  const b: ReadonlySet<number> = new Set([3]); // error: ts-syntax
  b.add(3); // currently no error because b is any, inconsistent with type-sig behavior
  const c: ReadonlyMap<number, string> = new Map([[3, '']]); // error: ts-syntax
  c.add(3, ''); // currently no error because c is any, inconsistent with type-sig behavior
  let d: NonNullable<string | null> = ''; // error: ts-syntax
  d = null; // currently no error because d is any, inconsistent with type-sig behavior
  const e: Readonly<{foo: string}> = {foo: ''}; // error: ts-syntax
  e.foo = ''; // currently no error because e is any, inconsistent with type-sig behavior
}

{
  const exported_with_ts_typenames = require('./exported');
  exported_with_ts_typenames.a.push(4); // error: readonly
  exported_with_ts_typenames.b.add(3); // error: readonly
  exported_with_ts_typenames.c.add(3, ''); // error: readonly
  exported_with_ts_typenames.d = null; // error: nonnull
  exported_with_ts_typenames.e.foo = ''; // error: readonly
}
