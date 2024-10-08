import * as React from 'react';

class Foo extends React.Component<{}, void> {}
class Bar extends React.Component<{}, void> {}

<Foo />; // OK
<Foo ref="foo" />; // Error: string ref is banned
<Foo ref={null} />; // OK
<Foo ref={undefined} />; // OK
<Foo ref={(foo: number) => {}} />; // Error: `Foo` is not a `number`.
<Foo ref={foo => (foo: Foo)} />; // Error: `Foo` may be null.
<Foo ref={foo => (foo: Foo | null)} />; // OK
<Foo ref={foo => (foo: Bar | null)} />; // Error: `Foo` is not `Bar`.

class FooExact extends React.Component<{||}, void> {}

<FooExact />; // OK
<FooExact ref="foo" />; // Error: string ref is banned
<FooExact ref={null} />; // OK
<FooExact ref={undefined} />; // OK
<FooExact ref={(foo: number) => {}} />; // Error: `FooExact` is not a `number`.
<FooExact ref={(foo: FooExact) => {}} />; // Error: `FooExact` may be null.
<FooExact ref={(foo: FooExact | null) => {}} />; // OK
<FooExact ref={(foo: Bar | null) => {}} />; // Error: `FooExact` is not `Bar`.
