const React = require('react');

type F<T> = (React$ElementConfig<T> => void) => void // error should not appear here

declare function foo<T>(T): (React$ElementConfig<T> => void) => void

declare function bar<P>(React$ComponentType<{ m: number, ...P}>): React$ComponentType<P>;

class C extends React.Component<{}> {}

(foo(bar(C)): F<typeof C>); // error on call

declare function spread<T: {}>(x: T): {...T, ...{}}; // error should not appear here

declare var inexact: {foo: number};
spread(inexact); // error on call
