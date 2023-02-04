// @flow

type F<T> = $Call<<R>((s: T) => R) => R, (s: T) => T>;
type App1<T> = $Call<<R>(x: R) => R, F<T>>;
type App2<T> = $Call<<R>(x: R) => R, App1<T>>;

("": F<string>);
(1: F<number>);

("": App2<string>);
(1: App2<number>);

("": App1<string>);
(1: App2<number>);

("": App1<number>); // Expected error: string incomp. with number
(1: App2<string>); // Expected error: number incomp. with string
