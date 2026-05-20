// @flow

interface IA {
//        ^
  mm(x: string): string;
  mm(x: number): number;
  mf : ((x: number) => number) & ((x: string) => string);
}

declare const i_a:
//            ^
    IA;
//  ^

i_a.mm("x");
//  ^
i_a.mm(1);
//  ^

i_a.mf("x");
//  ^
i_a.mf(1);

interface I {
  get y(): string;
//    ^
  set y(x: number): void;
//    ^
}

function* gen(): Generator<number, void, number> {
//        ^
  yield 1;
}

/*
 * Aliased polymorphic anonymous interface
 */
type PolyAnonAlias<X> = interface {
  f: X;
}

declare const polyAnonAlias: PolyAnonAlias<number>;
//            ^

/*
 * Generic interface that uses `this`
 */
interface Box<T> {
//        ^
  clone(): this;
  value(): T;
}

declare const box: Box<number>;
//            ^
const cloned = box.clone();
//    ^
const cloneFn = box.clone;
//    ^

declare const cls: Class<Box<number>>;
//            ^
