// @flow

var React = require('react');

class CustomComponent extends React.Component {
  props: {
    prop: string
  };
}

var a: ReactElement<{prop: string}> = <CustomComponent prop="asdf" />;
var b: ReactElement<{prop1: string}> = <CustomComponent prop="asdf" />; // Error: Props<{prop}> ~> Props<{prop1}>

<div id="asdf" />;
<div id={42} />; // Error: (`id` prop) number ~> string
var c: ReactElement<{id: string}> = <div id="asdf" />;
var d: ReactElement<{id: number}> = <div id="asdf" />; // Error: Props<{id:string}> ~> Props<{id:number}>
