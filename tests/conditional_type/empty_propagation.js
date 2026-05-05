type F<T> = T extends unknown ? T : empty;
declare var x: F<empty | number>;
x as empty; // error: number ~> empty;
