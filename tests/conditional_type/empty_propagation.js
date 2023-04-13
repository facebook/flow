type F<T> = T extends mixed ? T : empty;
declare var x: F<empty | number>;
(x: empty); // error: number ~> empty;
