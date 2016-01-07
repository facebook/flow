// @flow

var React = require('react');

class CustomComponent extends React.Component {
  props: {
    prop: string
  };
}

var a: ReactElement<any, {prop: string}, any> = <CustomComponent prop="asdf" />;
var b: ReactElement<any, {prop1: string}, any> = <CustomComponent prop="asdf" />; // Error: Props<{prop}> ~> Props<{prop1}>

<div id="asdf" />;
<div id={42} />; // Error: (`id` prop) number ~> string
var c: ReactElement<any, {id: string}, any> = <div id="asdf" />;
var d: ReactElement<any, {id: number}, any> = <div id="asdf" />; // Error: Props<{id:string}> ~> Props<{id:number}>
