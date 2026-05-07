type F<T> = T extends unknown ? T : empty;
declare const x: F<empty | number>;
x as empty; // error: number ~> empty;
