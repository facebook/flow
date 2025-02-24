const React = require('react');

class CustomComponent extends React.Component<{prop: string}, void> {}
class CustomComponentNope extends React.Component<{prop: string}, void> {}

var a: ExactReactElement_DEPRECATED<typeof CustomComponent> =
  <CustomComponent prop="asdf" />;
var b: ExactReactElement_DEPRECATED<typeof CustomComponentNope> =
  <CustomComponent prop="asdf" />; // Error: Bad class type
var c: ExactReactElement_DEPRECATED<Class<React.Component<{prop1: string}, void>>> =
  <CustomComponent prop="asdf" />; // Error: Props<{prop}> ~> Props<{prop1}>

var d: ExactReactElement_DEPRECATED<any> = <div id="asdf" />;
// We don't allow intrinsic elements to be viewed as React elements with
// different component types.
var e: ExactReactElement_DEPRECATED<'span'> = <div id="asdf" />;
// No error as long as expectations are consistent, though.
var f: ExactReactElement_DEPRECATED<'div'> = <div id="asdf" />;

const dataProps: {[StringPrefix<'data-'>]: string} = {};
const g = <div {...dataProps} />; // OK
