export type T = Readonly<{
    prop: ?Readonly<{
      mode: 'TAG',
  }>,
}>;

function foo(
  {prop}: T,
): boolean {
  if (prop == null) {
    return false;
  }
  return (
    prop.mode === 'TAG' &&
    true
  );
}
