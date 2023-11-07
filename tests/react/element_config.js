/**
 * @format
 * @flow
 */

import * as React from 'react';

declare var exactEmptyObject: {||};

class A extends React.Component<{p: ?number}> {
  static defaultProps: {p: ?number} = {p: 42}; // OK
}

class B extends React.Component<{p: ?number}> {
  static defaultProps: {p: ?number} = {p: 'foo'}; // Error: string ~> number
}

class C extends React.Component<{p: ?number}> {
  static defaultProps: {p?: number} = {} as {p?: number}; // OK
}

class D extends React.Component<{p: ?number}> {
  static defaultProps: {p?: string} = {} as {p?: string}; // OK
}

class E extends React.Component<{}> {
  static defaultProps: {} = {p: 42}; // OK
}

class F extends React.Component<{||}> {
  static defaultProps: {||} = {p: 42}; // Error: extra property `p`
}

class G extends React.Component<{p: ?number}> {
  static defaultProps: {p: ?number} = {p: 42}; // OK
}

class H extends React.Component<{p?: ?number}> {
  static defaultProps: {p: string} = {p: 'foo'}; // Error: string ~> number
}

class I extends React.Component<{p?: ?number}> {
  static defaultProps: {p?: number} = {} as {p?: number}; // OK
}

class J extends React.Component<{p?: ?number}> {
  static defaultProps: {p?: string} = {} as {p?: string}; // Error: string ~> number
}

({}) as React.ElementConfig<typeof A>; // OK
({p: 42}) as React.ElementConfig<typeof A>; // OK
({p: 'foo'}) as React.ElementConfig<typeof A>; // Error: string ~> number

({}) as React.ElementConfig<typeof B>; // OK
({p: 42}) as React.ElementConfig<typeof B>; // OK
({p: 'foo'}) as React.ElementConfig<typeof B>; // Error: string ~> number

({}) as {} as React.ElementConfig<typeof C>; // Error: missing property `p`
({p: 42}) as React.ElementConfig<typeof C>; // OK
({p: 'foo'}) as React.ElementConfig<typeof C>; // Error: string ~> number

({}) as {} as React.ElementConfig<typeof D>; // Error: missing property `p`
({p: 42}) as React.ElementConfig<typeof D>; // OK
({p: 'foo'}) as React.ElementConfig<typeof D>; // Error: string ~> number

({}) as React.ElementConfig<typeof E>; // OK
({p: 42}) as React.ElementConfig<typeof E>; // OK
({p: 'foo'}) as React.ElementConfig<typeof E>; // OK

exactEmptyObject as React.ElementConfig<typeof F>; // OK
({p: 42}) as React.ElementConfig<typeof F>; // Error: extra property `p`
({p: 'foo'}) as React.ElementConfig<typeof F>; // Error: extra property `p`

({}) as React.ElementConfig<typeof G>; // OK
({p: 42}) as React.ElementConfig<typeof G>; // OK
({p: 'foo'}) as React.ElementConfig<typeof G>; // Error: string ~> number

({}) as React.ElementConfig<typeof H>; // OK
({p: 42}) as React.ElementConfig<typeof H>; // OK
({p: 'foo'}) as React.ElementConfig<typeof H>; // Error: string ~> number

({}) as React.ElementConfig<typeof I>; // OK
({p: 42}) as React.ElementConfig<typeof I>; // OK
({p: 'foo'}) as React.ElementConfig<typeof I>; // Error: string ~> number

({}) as React.ElementConfig<typeof J>; // OK
({p: 42}) as React.ElementConfig<typeof J>; // OK
({p: 'foo'}) as React.ElementConfig<typeof J>; // Error: string ~> number
