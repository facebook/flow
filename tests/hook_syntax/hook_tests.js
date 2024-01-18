hook useCustom<T>(x: T): [T] { return [x]; }

const [v] = useCustom({a: 42});
v.a = 100; // Error, x is not writable
