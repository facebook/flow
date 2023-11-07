// @flow

type F<T> = $Call<<R>((s: T) => R) => R, (s: T) => T>;
type App1<T> = $Call<<R>(x: R) => R, F<T>>;
type App2<T> = $Call<<R>(x: R) => R, App1<T>>;

'' as F<string>;
1 as F<number>;

'' as App2<string>;
1 as App2<number>;

'' as App1<string>;
1 as App2<number>;

'' as App1<number>; // Expected error: string incomp. with number
1 as App2<string>; // Expected error: number incomp. with string
