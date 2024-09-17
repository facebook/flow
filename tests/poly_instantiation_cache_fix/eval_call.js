// @flow

type F<T> = ((s: T) => T) extends ((s: T) => infer R) ? R : empty;
type App1<T> = F<T> extends (infer R) ? R : empty;
type App2<T> = App1<T> extends (infer R) ? R : empty;

'' as F<string>;
1 as F<number>;

'' as App2<string>;
1 as App2<number>;

'' as App1<string>;
1 as App2<number>;

'' as App1<number>; // Expected error: string incomp. with number
1 as App2<string>; // Expected error: number incomp. with string
