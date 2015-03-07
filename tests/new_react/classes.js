var React = require('React');

class Foo extends React.Component {
    is_mounted: boolean;
    props: { x: number };

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
        nextContext: Object // nextContext?: Object
    ): void {
        this.qux();
    }
}

var foo = <Foo/>;
Foo.bar();
