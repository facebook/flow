// @flow

type X<T: number> = boolean;
(true: X<string>);
