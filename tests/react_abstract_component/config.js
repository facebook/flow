//@flow
const React = require('react');
type Props = {foo: number, bar: number, ...};
type DefaultProps = {foo: number, ...};
type Config = {+foo?: number, +bar: number, ...};

declare var x: Config;
declare var y: React.Config<Props, DefaultProps>;
x as React.Config<Props, DefaultProps>;
y as Config;

type NotTheRightConfig = {+baz: number, +qux: number, ...};
y as NotTheRightConfig; // Error, configs don't match

declare var z: NotTheRightConfig;
z as React.Config<Props, DefaultProps>; // Error, configs don't match

function HOC<Config: {...}, Instance>(
  x: component(ref: React.RefSetter<Instance>, ...Config),
): component(ref: React.RefSetter<Instance>, ...Config) {
  return x;
}

function HOC2<Props: {...}, DefaultProps: {...}, Instance>(
  x: component(ref: React.RefSetter<Instance>, ...React.Config<Props, DefaultProps>),
): component(ref: React.RefSetter<Instance>, ...React.Config<Props, DefaultProps>) {
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

const WrappedComponent2 = HOC2<{foo: number, bar: number, ...}, {foo: number, ...}, _>(
  Component,
);
const _f = <WrappedComponent2 />; // Error missing bar
