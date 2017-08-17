// @flow

import React from 'react';

declare var any: any;

React.createElement(); // Error: Needs a minimum of two arguments.
React.createElement('div'); // Error: Needs a minimum of two arguments.
React.createElement(42); // Error: Needs a minimum of two arguments.
React.createElement('div', {}); // OK
React.createElement(42, {}); // Error: Number is not a valid component type.
React.createElement({}, {}); // Error: Object is not a valid component type.
React.createElement(() => {}, {}); // OK

class A extends React.Component<{foo: number, bar: number}, void> {}
function B(props: {foo: number, bar: number}) {}

React.createElement(A, {foo: 1, bar: 2}); // OK
React.createElement(B, {foo: 1, bar: 2}); // OK
React.createElement(A, {
  foo: 42,
  bar: 'Hello, world!', // Error: `bar` is a string.
});
React.createElement(B, {
  foo: 42,
  bar: 'Hello, world!', // Error: `bar` is a string.
});
React.createElement(A, {foo: 42}); // Error: `bar` is missing.
React.createElement(B, {foo: 42}); // Error: `bar` is missing.
(React.createElement(A, {foo: 1, bar: 2}).type: Class<A>); // OK
(React.createElement(B, {foo: 1, bar: 2}).type: typeof B); // OK
(React.createElement(A, {foo: 1, bar: 2}).props.foo: number); // OK
(React.createElement(B, {foo: 1, bar: 2}).props.foo: number); // OK
(React.createElement(A, {foo: 1, bar: 2}).props.foo: boolean); // Error: `foo`
                                                               // is `number`.
(React.createElement(B, {foo: 1, bar: 2}).props.foo: boolean); // Error: `foo`
                                                               // is `number`.
React.createElement(A, {foo: 1, bar: 2}).nope; // Error: `nope` does not exist.
React.createElement(B, {foo: 1, bar: 2}).nope; // Error: `nope` does not exist.

class C extends React.Component<{foo: number, bar: number}, void> {
  static defaultProps = {bar: 42};
}
function D(props: {foo: number, bar: number}) {}
D.defaultProps = {bar: 42};

React.createElement(C, {foo: 1, bar: 2}); // OK
React.createElement(D, {foo: 1, bar: 2}); // OK
React.createElement(C, {
  foo: 42,
  bar: 'Hello, world!', // Error: `bar` is a string.
});
React.createElement(D, {
  foo: 42,
  bar: 'Hello, world!', // Error: `bar` is a string.
});
React.createElement(C, {foo: 42}); // OK: `bar` is in `defaultProps`.
React.createElement(D, {foo: 42}); // OK: `bar` is in `defaultProps`.
(React.createElement(C, {foo: 42}).props.bar: number); // OK
(React.createElement(D, {foo: 42}).props.bar: number); // OK

React.createElement(any, {whateverYouWant: 'yes'}); // OK
