type Get<M> = [M['x'], null];

type Args = Get<C>;

class B<T extends ReadonlyArray<unknown>> {
  x: number = 0;
}

class C extends B<Args> {}

declare const a: Args;
a[0]; // ok
