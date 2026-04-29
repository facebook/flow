---
title: Render Types
slug: /react/render-types
description: "How to use render types to constrain what a React component can render, enforcing composition rules at the type level."
---
Some component libraries or design systems may want to restrict how components may be composed.
For example, a Menu should only ever render MenuItems as children. Render types are a built-in way to support
these constraints while still affording users rich flexibility in how they use those components.

## Basic Behavior

A component can declare what it renders using the renders keyword:

```js flow-check
import * as React from 'react';

component Header(size: string, color: string) { return <div /> }

component LargeHeader(color: string) renders Header {
  return <Header size="large" color={color} />; // Ok!
}
```

When you declare that your component renders some specific element, you can return any component that eventually renders that component in its renders chain:

```js flow-check
import * as React from 'react';

component Header(size: string, color: string) { return <div /> }

component LargeHeader(color: string) renders Header {
  return <Header size="large" color={color} />;
}

component LargeBlueHeader() renders Header {
  // You could also use `renders LargeHeader` above
  return <LargeHeader color="blue" />;
}
```

Components can specify props that render specific elements:

```js flow-check
import * as React from 'react';

component Header(size: string, color: string, message: string) {
  return <h1 style={{color}}>{message}</h1>;
}

component Layout(header: renders Header) {
  return (
    <div>
      {header}
      <section>Hi</section>
    </div>
  );
}
```

And you can pass an element of either Header, or an element of a component that renders `Header`, to that prop:

```js
<Layout header={<LargeBlueHeader />} />;
```

You cannot pass a component that does not render a header to a render type expecting a header:

```js flow-check
import * as React from 'react';

component Footer() {
  return <footer />;
}

component Header(size: string, color: string, message: string) {
  return <h1 style={{color}}>{message}</h1>;
}

component Layout(header: renders Header) {
  return <div>{header}</div>;
}

<Layout header={<Footer />} />; // ERROR Footer does not render Header
```

## Integrating with a design system

Render types are designed to make integrating with a design system simple. If a prop in the design system component expects a render type, you can copy/paste that type onto your component to integrate with the design system:

```js flow-check
import * as React from 'react';

component Header() {
  return <h1>Header!</h1>;
}

component Layout(header: renders Header) {
  return <div>{header}</div>;
}

// Copy-paste the header props' type!
component ProductHeader() renders Header {
  // We must return a value that renders a Header to satisfy the signature
  return <Header />;
}

// And now you can integrate with the design system!
<Layout header={<ProductHeader />} />; // OK!
```

## Rendering Optional Elements

You may want to describe a component that can take a child that may eventually render an element or nothing. You can use a specialized render type variant `renders?` to achieve this:

```js flow-check
import * as React from 'react';

component DesignSystemCardFooter() {
  return <div>Footer Content</div>;
}

component DesignSystemCard(
  children: React.Node,
  footer: renders? DesignSystemCardFooter,
) {
  return <div>{children}{footer}</div>;
}

// With these definitions, all of the following work:

<DesignSystemCard footer={<DesignSystemCardFooter />}>Card</DesignSystemCard>;
<DesignSystemCard footer={null}>Card</DesignSystemCard>;
<DesignSystemCard footer={undefined}>Card</DesignSystemCard>;
<DesignSystemCard footer={false}>Card</DesignSystemCard>;

component ProductFooter(hasFooter?: boolean) renders? DesignSystemCardFooter {
  return hasFooter && <DesignSystemCardFooter />;
}

<DesignSystemCard footer={<ProductFooter />}>Card</DesignSystemCard>;
```

## Rendering Lists

You may want to describe a component that can take any amount of children that render a specific element as props. You can use a specialized render type variant `renders*` to achieve this:

```js flow-check
import * as React from 'react';

component DesignSystemMenuItem() {
  return <li>Menu Item</li>;
}

component DesignSystemMenu(
  children: renders* DesignSystemMenuItem,
) {
  return <ul>{children}</ul>
}

// With these definitions, all of the following work:

const menu1 = (
  <DesignSystemMenu>
    <DesignSystemMenuItem />
  </DesignSystemMenu>
);

const menu2 = (
  <DesignSystemMenu>
    <DesignSystemMenuItem />
    <DesignSystemMenuItem />
  </DesignSystemMenu>
);

const menu3 = (
  <DesignSystemMenu>
    {[
      <DesignSystemMenuItem />,
      <DesignSystemMenuItem />,
    ]}
    <DesignSystemMenuItem />
  </DesignSystemMenu>
);

component ProductMenuItem() renders DesignSystemMenuItem {
  return <DesignSystemMenuItem />;
}

const menu4 = (
  <DesignSystemMenu>
    {[
      <ProductMenuItem />,
      <DesignSystemMenuItem />,
    ]}
    <DesignSystemMenuItem />
  </DesignSystemMenu>
);
```

## Transparent Components

Components can be "transparent":

```js flow-check
import * as React from 'react';

component TransparentComponent<T extends React.Node>(children: T) renders T {
  // .. do something
  return children;
}

component Header(text: string) {
  return <h1>{text}</h1>;
}
component InstagramHeader() renders Header {
  return <Header text="Instagram" />;
}
component Layout(
  header: renders Header,
) {
  return <div>{header}</div>;
}

component Page() {
  const wrappedHeader = <TransparentComponent><InstagramHeader /></TransparentComponent>
  return <Layout header={wrappedHeader} />; // Ok!
}
```

## Interop with non-Component-Syntax components

You can use `renders` to annotate function components as well:

```js flow-check
import * as React from 'react';

component Header(text: string) {
  return <h1>{text}</h1>;
}
component InstagramHeader() renders Header {
  return <Header text="Instagram" />;
}
component Layout(
  header: renders Header,
) {
  return <div>{header}</div>;
}

function FunctionHeader(): renders Header {
  return <InstagramHeader />;
}

function InstagramPage() {
  return <Layout header={<FunctionHeader />} />; // OK!
}
```

## Subtyping Behavior

All render types are subtypes of both `React.Node` and `React.MixedElement`.

```js flow-check
import * as React from 'react';

component Header() {
 return <h1>Hello Header!</h1>;
}

declare const rendersHeader: renders Header;
declare const rendersMaybeHeader: renders? Header;
declare const rendersHeaderList: renders* Header;

rendersHeader as React.Node;
rendersMaybeHeader as React.Node;
rendersHeaderList as React.Node;

rendersHeader as React.MixedElement;
rendersMaybeHeader as React.MixedElement;
rendersHeaderList as React.MixedElement;
```

`renders Foo` is a subtype of `renders? Foo`, and `renders? Foo` is a subtype of `renders* Foo`.

```js flow-check
import * as React from 'react';

component Header() {
 return <h1>Hello Header!</h1>;
}

declare const rendersHeader: renders Header;
declare const rendersMaybeHeader: renders? Header;
declare const rendersHeaderList: renders* Header;

rendersHeader as renders? Header;
rendersHeader as renders* Header;
rendersMaybeHeader as renders* Header;

rendersMaybeHeader as renders Header; // ERROR
rendersHeaderList as renders Header; // ERROR
rendersHeaderList as renders? Header; // ERROR
```

## Common Issues

### Render types are opaque at runtime {#toc-render-types-opaque-at-runtime}

Render types are a type-only concept — you cannot inspect or compare component types at runtime. For example, you cannot check which component was passed to a render-typed prop:

```js
// This does NOT work — you can't compare component identity at runtime
if (icon === FancyIcon) { ... }
```

If you need to distinguish between different rendered components at runtime, use a [disjoint union](../types/unions.md#toc-disjoint-object-unions) wrapper instead:

```js flow-check
import * as React from 'react';

component FancyIcon() { return <span />; }
component BasicIcon() { return <span />; }

type IconProp =
  | {type: 'fancy', node: renders FancyIcon}
  | {type: 'basic', node: renders BasicIcon};

component Layout(icon: IconProp) {
  if (icon.type === 'fancy') {
    icon.node as renders FancyIcon; // Works!
  }
  return <div>{icon.node}</div>;
}
```

## See Also {#toc-see-also}

- [Unions](../types/unions.md) — render types interact with union types for flexible rendering
- [Subtypes](../lang/subtypes.md) — how render type subtyping works
