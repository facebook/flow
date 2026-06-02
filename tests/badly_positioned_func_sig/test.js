export type Bar = {readonly blah: number} extends {readonly blah: infer V} ? V : empty;

class X {
  foo: {bar: Bar} = {bar: 'cat'};
}
