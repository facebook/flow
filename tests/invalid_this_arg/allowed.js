type O = {
  x: number,
  m: (this: O, y: number) => void,
  n: (this: O) => void,
};

declare const o: O;

o.m.call(o, 1); // ok
o.m.apply(o, [1]); // ok
o.n.bind(o); // ok

const wrapper: {inner: O} = {inner: o};
wrapper.inner.m.call(wrapper.inner, 1); // ok
wrapper.inner.m.apply(wrapper.inner, [1]); // ok

class C {
  h: (this: C) => void = () => {};
  register(): void {
    this.h.bind(this); // ok
    this.h.call(this); // ok
  }
}

declare const maybe: ?O;
maybe?.m.call(maybe, 1); // ok
