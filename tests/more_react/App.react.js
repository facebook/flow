
/**
 * @providesModule App.react
 * @jsx React.DOM
 */

var React = require('react');

// expect args to be strings
function foo(p:string,q:string):string { return p+q; }

var App = React.createClass({

  getDefaultProps: function(): { y: string } {
    return {y:""}; // infer props.y: string
  },

  getInitialState: function() {
    return {z:0}; // infer state.z: number
  },

  handler: function() {
    this.setState({z:42}); // ok
  },

  render: function() {
    var x = this.props.x;
    var y = this.props.y;
    var z = this.state.z;

    //this.state;

    return (
      <div>
        {foo(x,y)}
        {foo(z,x)} // error, since z: number
      </div>
    );
  }

});


// JSX.js

var app =
  <App y={42}> // error, y: number but foo expects string in App.react
    Some text.
  </App>;


// API.react.js

app.setProps({y:42}); // error, y:number but foo expects string in App.react
app.setState({z:42}); // error, z:number but foo expects string in App.react

function bar(x:number) { }
bar(app.props.children); // No error, App doesn't specify propTypes so anything goes
