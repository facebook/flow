//@flow
const React = require('react');
type Props = {foo: number, bar: number, ...};
type DefaultProps = {foo: number, ...};
type Config = {+foo?: number, +bar: number, ...};

declare var x: Config;

type NotTheRightConfig = {+baz: number, +qux: number, ...};
x as NotTheRightConfig; // Error, configs don't match

function HOC<Config: {...}, Instance>(
  x: component(ref: React.RefSetter<Instance>, ...Config),
): component(ref: React.RefSetter<Instance>, ...Config) {
  return x;
}

class Component extends React.Component<{foo: number, bar: number, ...}> {
  static defaultProps: {foo: number, ...} = {foo: 3};
}

const WrappedComponent = HOC(Component);

// Make sure all props are correctly required
const _a = <WrappedComponent foo={3} bar={3} />;
const _b = <WrappedComponent bar={3} />;
const _c = <WrappedComponent foo={3} />; // Error missing bar
