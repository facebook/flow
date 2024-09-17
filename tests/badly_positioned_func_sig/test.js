export type Bar = {+blah: number} extends {+blah: infer V} ? V : empty;

class X {
  foo: {|bar: Bar|} = {bar: 'cat'};
}
