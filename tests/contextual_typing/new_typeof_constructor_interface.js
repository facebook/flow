// `Box` is a `declare var` of a constructor interface, so a binding annotated
// `typeof Box` still hints the executor's parameters.
interface Box<R> {
  value: R;
}
interface BoxConstructor {
  new <R = unknown>(executor: (resolve: (value: R) => void) => unknown): Box<R>;
}
declare var Box: BoxConstructor;

const B: typeof Box = Box;

new B(resolve => { // ok
  resolve(1);
});

new B<number>(resolve => {
  resolve('s'); // error: string ~> number
});
