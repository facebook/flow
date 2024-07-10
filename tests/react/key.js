import React from 'react';

class Foo extends React.Component<{}, void> {}

<Foo />; // OK
<Foo key="42" />; // OK
<Foo key={42} />; // OK
<Foo key={null} />; // OK
<Foo key={undefined} />; // OK
<Foo key={true} />; // Error

class FooExact extends React.Component<{||}, void> {}

<FooExact />; // OK
<FooExact key="42" />; // OK
<FooExact key={42} />; // OK
<FooExact key={null} />; // OK
<FooExact key={undefined} />; // OK
<FooExact key={true} />; // Error

<Foo {...{key: 42}} />; // error
{
    declare const keyProps: {foo: number} | {key: string};
    <Foo {...keyProps} />; // error
}
{
    declare const keyProps: $ReadOnly<{foo: number} | {key: string}>;
    <Foo {...keyProps} />; // error
}
{
    declare const keyProps: { foo: $ReadOnly<{foo: number} | {key: string}>};
    <Foo {...keyProps.foo} />; // error
}
{
    type PropsWithKey = {| key: string |};
    declare const keyProps: { foo: number, ...PropsWithKey};
    <Foo {...keyProps} />; // error
}
