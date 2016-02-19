// @flow

var React = require('react');

class CustomComponent extends React.Component {
  props: {
    prop: string
  };
}

var a: ReactElement<{prop: string}> = <CustomComponent prop="asdf" />;
var b: ReactElement<{prop1: string}> = <CustomComponent prop="asdf" />; // Error: Props<{prop}> ~> Props<{prop1}>

// Since intrinsics are typed as `any` out of the box, we can pass any
// attributes to intrinsics!
var c: ReactElement<any> = <div not_a_real_attr="asdf" />;
// However, we don't allow such elements to be viewed as React elements with
// different attributes.
var d: ReactElement<{doesntmatch: string}> = <div not_a_real_attr="asdf" />;
// No error as long as expectations are consistent, though.
var e: ReactElement<{not_a_real_attr: string}> = <div not_a_real_attr="asdf" />;
