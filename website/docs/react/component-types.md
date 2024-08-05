---
title: Component Types
slug: /react/component-types
---
:::info
Component Types are only available in Flow v0.243.0+. If you are on an older version, please use [React.AbstractComponent](../types#toc-react-abstractcomponent)
:::

Component Types have syntax similar to our runtime [Component Syntax](../component-syntax) to make it easy to describe
the type of a component. Component Types are most useful for writing library definitions.

## Specifying Props
To declare a Component Type you can use the `component` keyword and list out the props your component expects.

```js flow-check
import * as React from 'react';

type ComponentType = component(numberProp: number, optionalProp?: string);

declare const Component: ComponentType;

<Component numberProp={3} />; // OK! optionalProp is optional
```

Like [Component Syntax](../component-syntax/#rest-parameters), Component Types also accept a rest parameter:
```js
import * as React from 'react';

import type {Props as StarProps} from './Star';
import Star from './Star';

type BlueStarType = component(specificProp: string, ...StarProps);
```

Like Component Syntax, you can also declare an inline ref prop (but not in your rest parameter):
```js flow-check
import * as React from 'react';
import {useRef} from 'react';

type ComponentWithRef = component(someProp: number, ref: React.RefSetter<number>);

declare const Component: ComponentWithRef;

component Example() {
    const ref = useRef<number | null>(null);
    return <Component someProp={42} ref={ref}/>;
}
```
## Specifying Render Types

You can also specify the [Render Type](../render-types) for your component just like you can with
[Component Syntax](../render-types/#basic-behavior)

```js flow-check
import * as React from 'react';

component Foo() { return null }
type ComponentWithRenders = component() renders Foo;

declare const Component: ComponentWithRenders;
<Component /> as renders Foo; // OK!
```

## Polymorphic Component Types

You can also write polymorphic Component Types, which is helpful for declaring "transparent" components:

```js flow-check
import * as React from 'react';

declare const TransparentComponent: component<T: React.Node>(children: T) renders T;

component Example() { return null }

const element: renders Example = (
    <TransparentComponent>
        <Example />
    </TransparentComponent>
); // OK!
```

## Annotating Components with Component Types

Here's how you can describe the type of a Component Syntax component using a Component Type:
```js flow-check
import * as React from 'react';

component Foo() { return null }

component Example(someProp: number, ref: React.RefSetter<number>) renders Foo {
    return <Foo />;
}

Example as component(someProp: number, ref: React.RefSetter<number>) renders Foo; // OK!


component PolymorphicExample<T: React.Node>(children: T) renders T {
    return children;
}

PolymorphicExample as component<T: React.Node>(children: T) renders T; // OK!
```
