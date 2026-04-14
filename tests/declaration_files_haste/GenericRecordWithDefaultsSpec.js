export type Spec = {
  foo: string,
  bar: ?string,
  ...
};

export const defaults: Partial<Spec> = {
  foo: 'foo',
};
