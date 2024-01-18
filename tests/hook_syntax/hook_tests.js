hook useCustom<T>(x: T): [T] { return [x]; }

const [v] = useCustom({a: 42});
v.a = 100; // Error, x is not writable

useCustom as <T>(T) => [T]; // error, hook and nonhook incompatible
useCustom as hook <T>(T) => [T]; // ok

declare const nonhook: <T>(T) => [T];
nonhook as typeof useCustom; // error, nonhook and hook incompatible

hook useCustom2<T>(x: T): [T] { return [x];}
useCustom as typeof useCustom2; // error, hooks unique

useCustom as typeof useCustom; // ok
