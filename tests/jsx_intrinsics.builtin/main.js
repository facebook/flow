// @flow

var React = require('react');

class CustomComponent extends React.Component {
  props: {
    prop: string
  };
}

var a: ReactElement<any, {prop: string}, any> = <CustomComponent prop="asdf" />;
var b: ReactElement<any, {prop1: string}, any> = <CustomComponent prop="asdf" />; // Error: Props<{prop}> ~> Props<{prop1}>

// Since intrinsics are typed as `any` out of the box, we can pass any
// attributes to intrinsics!
var c: ReactElement<any, any, any> = <div not_a_real_attr="asdf" />;
// However, we don't allow such elements to be viewed as React elements with
// different attributes.
var d: ReactElement<any, {doesntmatch: string}, any> = <div not_a_real_attr="asdf" />;
// No error as long as expectations are consistent, though.
var e: ReactElement<any, {not_a_real_attr: string}, any> = <div not_a_real_attr="asdf" />;
