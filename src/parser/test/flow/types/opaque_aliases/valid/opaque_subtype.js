opaque type Counter: Box<T> = Container<T>;
opaque type Counter super empty extends Box<T> = Container<T>;
opaque type Counter super Box<T> = Container<T>;
