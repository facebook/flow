//@flow


export class C<Y> {
    x: typeof Y;
}

class X {
    p: string;
};
const x = X;

export function f<X>(): typeof X { return x; } // no longer errors, X resolves to class in typeof X
