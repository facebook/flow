//@flow

async function f() {
  await (null as any);
}

class C<T> {
  foo: T;
}

declare function instToAny<T>(x: (y: C<T>) => mixed): (C<T>) => mixed;

instToAny(null as any);

type PolyInlineInst<T> = interface {foo: T};
declare function anyToInlineInst<T>(x: PolyInlineInst<T>): T;

anyToInlineInst(null as any);

type PolyObj<T> = {foo: T};

declare function g<T>(x: PolyObj<T>): T;
g(null as any);

declare function h<T>(x: {foo: PolyObj<T>}): T;
h(null as any);

declare function i<T>(x: (y: PolyObj<T>) => mixed): (PolyObj<T>) => mixed;
i(null as any);

type Foo = number;
declare function j<X>(): <Y: X>(x: {foo: Y}) => Y;
j<Foo>()(null as any);
