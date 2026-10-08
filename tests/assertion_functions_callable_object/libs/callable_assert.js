declare module 'callable-assert' {
  declare module.exports: {
    (value: unknown): asserts value,
    extra: number,
  };
}
