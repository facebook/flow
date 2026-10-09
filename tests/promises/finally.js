declare const p: Promise<number>;

// The callback is optional and may be null or undefined
p.finally();
p.finally(null);
p.finally(undefined);

// The callback may return a value, which is ignored
p.finally(() => 1);
p.finally(async () => {});
p.finally(() => {}) as Promise<number>;
p.finally(() => {}) as Promise<string>; // Error: the resolved value is unchanged

p.finally(1); // Error: not a function

// An override must accept everything `Promise.prototype.finally` does
declare class NarrowFinally<R> extends Promise<R> {
  finally(onFinally: () => unknown): NarrowFinally<R>; // Error: `null` and `undefined` are not functions
}
declare class WideFinally<R> extends Promise<R> {
  finally(onFinally?: (() => unknown) | void | null): WideFinally<R>; // ok
}
