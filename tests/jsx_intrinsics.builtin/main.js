const React = require('react');

class CustomComponent extends React.Component<{prop: string}, void> {}
class CustomComponentNope extends React.Component<{prop: string}, void> {}

var a: ExactReactElement_DEPRECATED<typeof CustomComponent> =
  <CustomComponent prop="asdf" />;
var b: ExactReactElement_DEPRECATED<typeof CustomComponentNope> =
  <CustomComponent prop="asdf" />; // Error: Bad class type
var c: ExactReactElement_DEPRECATED<Class<React.Component<{prop1: string}, void>>> =
  <CustomComponent prop="asdf" />; // Error: Props<{prop}> ~> Props<{prop1}>

// Since intrinsics are typed as `any` out of the box, we can pass any
// attributes to intrinsics!
var d: ExactReactElement_DEPRECATED<any> = <div not_a_real_attr="asdf" />;
// However, we don't allow such elements to be viewed as React elements with
// different component types.
var e: ExactReactElement_DEPRECATED<'span'> = <div not_a_real_attr="asdf" />;
// No error as long as expectations are consistent, though.
var f: ExactReactElement_DEPRECATED<'div'> = <div not_a_real_attr="asdf" />;
