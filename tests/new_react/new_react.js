var React = require('React');
var C = React.createClass({
    propTypes: {
        x: React.PropTypes.string.isRequired,
        y: React.PropTypes.array,
        z: React.PropTypes.number
    },
    replaceProps(props: { }) { },

    getDefaultProps() {
        return { z: 0 };
    },
    getInitialState() { return 4; },
    render() {
        var foo: string = this.state;
        var bar: string = this.props;
        var qux: string = this.props.z;
        var w:number = this.props.x;
        this.props.y[0];
        var len:number = this.props.x.length;
        return <div/>;
    }

})

var element = <C x = {0}/>;

var x: number = C.displayName;
