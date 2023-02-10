// @flow

declare opaque type A;
declare opaque type B;
declare opaque type C;
declare var a: A;
declare var b: B;
declare var c: C;

declare function fn1<T>(x: T): T;
declare function fn2<T>(x: (T) => void): T => void;
declare function fn3<T>(x: T, y: (T) => void): T;
declare function fn4<T>(x: T, y: (T) => void): T => void;

declare function fn5<T>(x: T): $NonMaybeType<T>;
declare function fn6<T>(x: (T) => void): ($NonMaybeType<T>) => void;
declare function fn7<T>(x: T, y: (T) => void): $NonMaybeType<T>;
declare function fn8<T>(x: T, y: (T) => void): ($NonMaybeType<T>) => void;

(fn1(a): B); // Error
(fn2((a: A) => {}): B => void); // Error
(fn3(a, (b: B) => {}): C); // Error
(fn4(a, (b: B) => {}): C => void); // Error

(fn5(a): B); // Error
(fn6((a: A) => {}): B => void); // Error
(fn7(a, (b: B) => {}): C); // Error
(fn8(a, (b: B) => {}): C => void); // Error
