const React = require('react');

class CustomComponent extends React.Component<{prop: string}, void> {}
class CustomComponentNope extends React.Component<{prop: string}, void> {}

var a: ExactReactElement_DEPRECATED<typeof CustomComponent> =
  <CustomComponent prop="asdf" />;
var b: ExactReactElement_DEPRECATED<typeof CustomComponentNope> =
  <CustomComponent prop="asdf" />; // Error: Bad class type
var c: ExactReactElement_DEPRECATED<Class<React.Component<{prop1: string}, void>>> =
  <CustomComponent prop="asdf" />; // Error: Props<{prop}> ~> Props<{prop1}>

const dataProps: {[StringPrefix<'data-'>]: string} = {};
const d = <div {...dataProps} />; // OK
