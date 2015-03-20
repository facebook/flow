var React = require('React');

class Foo extends React.Component {
    is_mounted: boolean;
    props: { x: number };
    state: { y: number };

    static bar(): void {}

    qux(): void {
        var _: string = this.props.x;
    }

    getInitialState(): { y: string } {
        return { y: "" };
    }

    setState(o: { y_: string }): void { }

    componentDidMount(): void {
        this.is_mounted = true;
    }

    componentWillReceiveProps(
        nextProps: Object,
        nextContext: any
    ): void {
        this.qux();
    }
}

Foo.defaultProps = 0;
var foo = <Foo/>;

Foo.bar();
