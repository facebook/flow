var React = require('react');

type DefaultProps = { };
type Props = { x: number };
type State = { y: number };

class Foo extends React.Component<Props, State> {
  static defaultProps: DefaultProps;

  is_mounted: boolean;

  static bar(): void {}

  qux(): void {
    var _: string = this.props.x;
  }

  constructor(props: Props) {
    super(props);
    this.state = { y: "" };
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
var foo: $jsx<number> = <Foo/>;

Foo.bar();
