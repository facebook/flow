//@flow

async function f() {
  await (null : any); // Error for now
}

class C<T> {
  foo: T;
}

declare function instToAny<T>(x: (y: C<T>) => mixed): C<T> => mixed;

instToAny((null: any)); // Error for now

type PolyObj<T> = {foo: T};

declare function g<T>(x: PolyObj<T>): T;
g((null: any));

declare function h<T>(x: {foo: PolyObj<T>}): T;
h((null: any));


declare function i<T>(x: (y: PolyObj<T>) => mixed): PolyObj<T> => mixed;
i((null: any));
