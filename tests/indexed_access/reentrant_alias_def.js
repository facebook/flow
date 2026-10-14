type Get<M> = [M['x'], null];

export type Args = Get<C>;

class B<T extends ReadonlyArray<unknown>> {
  x: number = 0;
}

class C extends B<Args> {}
