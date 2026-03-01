type T = {
  foo?(): void;
  bar?(): string;
  baz?(prop: string, fn: (prop: string) => boolean): boolean;
  withTypeParams?<T>(x: T): T;
};
