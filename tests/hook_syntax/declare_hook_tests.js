declare hook useCustom<T>(x: T): [T];

const [v] = useCustom({a: 42});
v.a = 100; // Error, x is not writable

useCustom as <T>(T) => [T]; // error, hook and nonhook incompatible
useCustom as hook <T>(T) => [T]; // ok

declare const nonhook: <T>(T) => [T];
nonhook as typeof useCustom; // error, nonhook and hook incompatible

declare hook useCustom2<T>(x: T): [T];
useCustom as typeof useCustom2; // ok, hook annotations compatible

useCustom as typeof useCustom; // ok
