// @flow

var React = require('react');

class CustomComponent extends React.Component {
  props: {
    prop: string
  };
}

var a: ReactElement<any, {prop: string}, any> = <CustomComponent prop="asdf" />;
var b: ReactElement<any, {prop1: string}, any> = <CustomComponent prop="asdf" />; // Error: Props<{prop}> ~> Props<{prop1}>

// Not an error because intrinsics are typed as `any` out of the box!
var c: ReactElement<any, {doesntmatch: string}, any> = <div not_a_real_attr="asdf" />;
