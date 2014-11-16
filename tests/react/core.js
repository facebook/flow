var FooReact = React.createClass({
    getDefaultProps():number { return 0; },
    setProps(x:string):void { },

    getInitialState() { return 0; },
    setState(x) { this.state = "..."; },

    render() { this.state / 42; return <div/>; }
});

var foo = <FooReact/>;

var BarReact = React.createClass({
    propTypes: { x: React.PropTypes.number.isRequired },
    render() { this.props.x / 42; return <div/>; }
});

var bar = <BarReact x = {".."}/>;
bar.state;

var x:number = BarReact.displayName;

React.render();

class QuxReact extends ReactComponent<{},{x:number},any> {
    static getDefaultProps():{x:number} { return {x:0}; }
}

var qux = <FooReact/>;
