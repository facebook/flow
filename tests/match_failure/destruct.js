// @flow

export type T = $ReadOnly<{
    prop: ?$ReadOnly<{
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
